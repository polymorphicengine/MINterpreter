module InterpretMINI where

import ParseMINI

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Value = Integer
type Env = Map.Map Name Value

type StateID = StateT Env Identity

evalExpNest:: Env -> ExpressionNested -> StateID Value
evalExpNest env (ENum i) = return i
evalExpNest env (EVar (Var name)) = case Map.lookup name env of
                                          Nothing -> error "undefined variable call"
                                          (Just val) -> return val
evalExpNest env (Exp expr) = evalExp env expr

evalExp :: Env -> Expression -> StateID Value
evalExp env (Pos expr) = evalExpNest env expr
evalExp env (Neg expr) =  do
                num <- evalExpNest env expr
                return (- num)
evalExp env (Term exp1 op exp2) = do
                            num1 <- evalExpNest env exp1
                            num2 <- evalExpNest env exp2
                            return $ (getOp op) num1 num2
                            where getOp Plus = (+)
                                  getOp Minus = (-)
                                  getOp Times = (*)
                                  getOp Divide = (div)

evalBool :: Env -> Boolean -> StateID Bool
evalBool env (BExp exp1 rel exp2) = do
                            num1 <- evalExp env exp1
                            num2 <- evalExp env exp2
                            return $ (getRel rel) num1 num2
                            where getRel GEQ = (>=)
                                  getRel LEQ = (<=)
                                  getRel EQQ = (==)
                                  getRel NEQ = (/=)
                                  getRel LE = (<)
                                  g1etRel GE = (>)

assignEval :: Assign -> StateID ()
assignEval (Ass (Var name) expr) = do
                              env <- get
                              num <- evalExp env expr
                              put (Map.insert name num env)

ifEval:: If -> StateID ()
ifEval (If boolExp stats) = do
                      env <- get
                      bool <- evalBool env boolExp
                      if bool then
                        statsEval stats
                      else
                        return ()
ifEval (Elif boolExp stats1 stats2) = do
                      env <- get
                      bool <- evalBool env boolExp
                      if bool then
                        statsEval stats1
                      else
                        statsEval stats2

whileEval:: While -> StateID ()
whileEval w@(While boolExp stats) = do
                      env <- get
                      bool <- evalBool env boolExp
                      if bool then
                        do
                          statsEval stats
                          whileEval w
                      else
                        return ()

statEval:: Statement -> StateID ()
statEval (WSt w) = whileEval w
statEval (ISt i) = ifEval i
statEval (ASt a) = assignEval a

statsEval :: Statements -> StateID ()
statsEval Eps = return ()
statsEval (St stat stats) = do
                        statEval stat
                        statsEval stats

returnEval :: Env -> Return -> StateID Value
returnEval env (Return (Var name)) = case Map.lookup name env of
                                          Nothing -> error "undefined variable call"
                                          (Just val) -> return val

procedureEval:: Procedure -> StateID Value
procedureEval (Proc stats ret) = do
                              statsEval stats
                              env' <- get
                              returnEval env' ret

argsToList:: Arguments -> [Var]
argsToList (Arg v) = [v]
argsToList (Args v vs) = v:(argsToList vs)

-- We can throw an error here if the amount of arguments is not equal to the amount if inputs
argumentEval:: Arguments -> [Integer] -> Env
argumentEval args input | length input /= length argList = error "length of input does not match length of arguments"
                        | otherwise = Map.unions $ zipWith (\(Var name) i -> Map.insert name i Map.empty) argList input
                      where argList = argsToList args

runProgram :: [Integer] -> Program -> Value
runProgram inputs (Prog args procedure) = runIdentity $ evalStateT (procedureEval procedure) (argumentEval args inputs)

-- TESTS


-- runIdentity $(evalStateT $ evalExp Map.empty (Pos (ENum 5))) Map.empty
-- runIdentity $(evalStateT $ evalExp Map.empty ( (Term (Exp (Term (ENum 1) Plus (ENum 2))) Times (ENum 3)) )) Map.empty
-- runIdentity $ runStateT  (assignEval (Ass (Var "x") (Pos (ENum 5)))) Map.empty



---(St (ASt (Ass (Var "c") (Term (ENum 5) Minus (ENum 2)))) (St (ASt (Ass (Var "d") (Pos (EVar (Var "c"))))) Eps))
-- runIdentity $ runStateT  (statsEval (St (ASt (Ass (Var "c") (Term (ENum 5) Minus (ENum 2)))) (St (ASt (Ass (Var "d") (Pos (EVar (Var "c"))))) Eps))) Map.empty

-- (St (ASt (Ass (Var "x") (Pos (ENum 1)))) (St (WSt (While (BExp (Pos (EVar (Var "x"))) EQQ (Pos (ENum 1))) (St (ASt (Ass (Var "x") (Term (EVar (Var "x")) Minus (ENum 1)))) Eps))) Eps))
-- runIdentity $ runStateT  (statsEval (St (ASt (Ass (Var "x") (Pos (ENum 1)))) (St (WSt (While (BExp (Pos (EVar (Var "x"))) EQQ (Pos (ENum 1))) (St (ASt (Ass (Var "x") (Term (EVar (Var "x")) Minus (ENum 1)))) Eps))) Eps))) Map.empty

-- (Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LE (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "c") (Pos (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "c") (Pos (EVar (Var "b"))))) Eps))) Eps) (Return (Var "c"))))

-- runIdentity $ runStateT  (programEval [1,0] (Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LE (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "c") (Pos (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "c") (Pos (EVar (Var "b"))))) Eps))) Eps) (Return (Var "c")))) ) Map.empty

-- divide by zero example
-- (Prog (Arg (Var "a")) (Proc (St (ASt (Ass (Var "b") (Term (EVar (Var "a")) Divide (ENum 0)))) Eps) (Return (Var "b"))))
