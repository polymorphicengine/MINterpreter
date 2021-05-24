module InterpretMINI where

{-| ------------------------
            import
-}  ------------------------

import ParseMINI

import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type Value = Integer
type Env = Map.Map Name Value -- mapping from names to values
type StateID = StateT Env Identity -- handling states

{-| ---------------------------------------
        well-formed-expression interpreter
-}  ---------------------------------------

-- nested expression
expNestEval:: ExpressionNested -> StateID Value
expNestEval (ENum i) = return i
expNestEval (EVar (Var name)) = do
                          env <- get
                          case Map.lookup name env of
                                    Nothing -> error "undefined variable call"
                                    (Just val) -> return val
expNestEval (Exp expr) = expEval expr

-- combined expression
expEval :: Expression -> StateID Value
expEval (Pos expr) = expNestEval expr
expEval (Neg expr) =  do
                num <- expNestEval expr
                return (- num)
expEval (Term exp1 op exp2) = do
                        num1 <- expNestEval exp1
                        num2 <- expNestEval exp2
                        return $ (getOp op) num1 num2
                        where getOp Plus = (+)
                              getOp Minus = (-)
                              getOp Times = (*)
                              getOp Divide = (div)

-- boolean expression
boolEval :: Boolean -> StateID Bool
boolEval (BExp exp1 rel exp2) = do
                      num1 <- expEval exp1
                      num2 <- expEval exp2
                      return $ (getRel rel) num1 num2
                      where getRel GEQ = (>=)
                            getRel LEQ = (<=)
                            getRel EQQ = (==)
                            getRel NEQ = (/=)
                            getRel LE = (<)
                            g1etRel GE = (>)

{-| ---------------------------------------
            statement interpreter
-}  ---------------------------------------

-- variable assignment
assignEval :: Assign -> StateID ()
assignEval (Ass (Var name) expr) = do
                            env <- get
                            num <- expEval expr
                            put (Map.insert name num env)

-- if-or-ifelse statement
ifEval:: If -> StateID ()
ifEval (If boolExp stats) = do
                      bool <- boolEval boolExp
                      if bool then
                        statsEval stats
                      else
                        return ()
ifEval (Elif boolExp stats1 stats2) = do
                      bool <- boolEval boolExp
                      if bool then
                        statsEval stats1
                      else
                        statsEval stats2

-- while statement
whileEval:: While -> StateID ()
whileEval w@(While boolExp stats) = do
                      bool <- boolEval boolExp
                      if bool then
                        do
                          statsEval stats
                          whileEval w
                      else
                        return ()

-- statement
statEval:: Statement -> StateID ()
statEval (WSt w) = whileEval w
statEval (ISt i) = ifEval i
statEval (ASt a) = assignEval a

-- statements
statsEval :: Statements -> StateID ()
statsEval Eps = return ()
statsEval (St stat stats) = do
                        statEval stat
                        statsEval stats

{-| --------------------------
            program
-}  --------------------------

-- return statement
returnEval :: Return -> StateID Value
returnEval (Return (Var name)) = do
                          env <- get
                          case Map.lookup name env of
                                Nothing -> error "undefined variable call"
                                (Just val) -> return val

-- procedure statement
procedureEval:: Procedure -> StateID Value
procedureEval (Proc stats ret) = do
                              statsEval stats
                              returnEval ret

-- procedure arguments
argumentEval:: Arguments -> [Integer] -> Env
argumentEval args input | length input /= length argList = error "length of input does not match length of arguments"
                        | otherwise = Map.unions $ zipWith (\(Var name) i -> Map.insert name i Map.empty) argList input
                      where argList = argsToList args

-- program
runProgram :: [Integer] -> Program -> Value
runProgram inputs (Prog args procedure) = genRun procedureEval procedure (argumentEval args inputs)


{-| --------------------------
       auxiliary functions
-}  --------------------------

-- procedure arguments
argsToList:: Arguments -> [Var]
argsToList (Arg v) = [v]
argsToList (Args v vs) = v:(argsToList vs)

genRun :: (b -> StateID a) -> b -> Env -> a
genRun eval input env = runIdentity $ evalStateT (eval input) env



-- T O  D E L E T E

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

-- greatest common divisor example
--(Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ASt (Ass (Var "r") (Pos (EVar (Var "b"))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) NEQ (Pos (ENum 0))) (St (WSt (While (BExp (Pos (EVar (Var "b"))) NEQ (Pos (ENum 0))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "b") (Term (EVar (Var "b")) Minus (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "a") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps))) Eps))) (St (ASt (Ass (Var "r") (Pos (EVar (Var "a"))))) Eps)))) Eps)) (Return (Var "r"))))
-- runProgram [42,56] (Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ASt (Ass (Var "r") (Pos (EVar (Var "b"))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) NEQ (Pos (ENum 0))) (St (WSt (While (BExp (Pos (EVar (Var "b"))) NEQ (Pos (ENum 0))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "b") (Term (EVar (Var "b")) Minus (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "a") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps))) Eps))) (St (ASt (Ass (Var "r") (Pos (EVar (Var "a"))))) Eps)))) Eps)) (Return (Var "r"))))


-- N E W   progs

-- factorial of n
-- runProgram [10] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "i") (Pos (ENum 1)))) (St (ASt (Ass (Var "fac") (Pos (ENum 1)))) (St (ISt (Elif (BExp (Pos (EVar (Var "n"))) LE (Pos (ENum 0))) (St (ASt (Ass (Var "fac") (Pos (ENum 0)))) Eps) (St (WSt (While (BExp (Pos (EVar (Var "n"))) GEQ (Pos (EVar (Var "i")))) (St (ASt (Ass (Var "fac") (Term (EVar (Var "fac")) Times (EVar (Var "i"))))) (St (ASt (Ass (Var "i") (Term (EVar (Var "i")) Plus (ENum 1)))) Eps)))) Eps))) Eps))) (Return (Var "fac"))))

-- reversal of n
-- runProgram [123456789] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "reverse") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "rem") (Term (EVar (Var "n")) Minus (Exp (Term (ENum 10) Times (Exp (Term (EVar (Var "n")) Divide (ENum 10)))))))) (St (ASt (Ass (Var "reverse") (Pos (Exp (Term (Exp (Term (EVar (Var "reverse")) Times (ENum 10))) Plus (EVar (Var "rem"))))))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) Eps))))) Eps)) (Return (Var "reverse"))))

-- count digits of n
-- runProgram [71283719823791273] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "counter") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) (St (ASt (Ass (Var "counter") (Term (EVar (Var "counter")) Plus (ENum 1)))) Eps)))) Eps)) (Return (Var "counter"))))
