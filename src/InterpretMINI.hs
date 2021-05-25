module InterpretMINI where

{-| ------------------------
            import
-}  ------------------------

import ParseMINI

import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type Value = Either Integer Procedure
type Env = Map.Map Name Value -- mapping from names to values
type StateID = StateT Env Identity -- handling states

{-| ---------------------------------------
        well-formed-expression interpreter
-}  ---------------------------------------


-- nested expression
expNestEval:: ExpressionNested -> StateID Value
expNestEval (ECall (Call (Var name) argList)) = do
                          env <- get
                          case Map.lookup name env of
                                    Nothing -> error "undefined procedure call"
                                    (Just (Left val) ) -> error "something went wrong"
                                    (Just (Right p@(Proc _ args _))) -> do
                                                                  val <- procedureEval inputs p
                                                                  put $ restoreEnv env args       --restore the environment
                                                                  return val
                                     where inputs = valuesToInts $ evalExpressions env (argsListToExp argList)

expNestEval (ENum i) = return (Left i)

expNestEval (EVar (Var name)) = do
                          env <- get
                          case Map.lookup name env of
                                    Nothing -> error "undefined variable call"
                                    (Just (Left val) ) -> return (Left val)
                                    (Just (Right procedure)) -> error "something went wrong"

expNestEval (Exp expr) = expEval expr

-- combined expression
expEval :: Expression -> StateID Value
expEval (Pos expr) = expNestEval expr
expEval (Neg expr) =  do
                val <- expNestEval expr
                case val of
                    (Left num) -> return $ Left (- num)
                    (Right p) -> error ""
expEval (Term exp1 op exp2) = do
                        val1 <- expNestEval exp1
                        val2 <- expNestEval exp2
                        case val1 of
                          (Left num1) -> case val2 of
                                  (Left num2) -> return $ Left ((getOp op) num1 num2)
                                  (Right p) -> error ""
                          (Right p) -> error ""
                        where getOp Plus = (+)
                              getOp Minus = (-)
                              getOp Times = (*)
                              getOp Divide = (div)

-- boolean expression
boolEval :: Boolean -> StateID Bool
boolEval (BExp exp1 rel exp2) = do
                      val1 <- expEval exp1
                      val2 <- expEval exp2
                      case val1 of
                        (Left num1) -> case val2 of
                                (Left num2) -> return $ (getRel rel) num1 num2
                                (Right p) -> error ""
                        (Right p) -> error ""
                      where getRel GEQ = (>=)
                            getRel LEQ = (<=)
                            getRel EQQ = (==)
                            getRel NEQ = (/=)
                            getRel LE = (<)
                            getRel GE = (>)

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
                                (Just (Left val) ) -> return (Left val)
                                (Just (Right procedure)) -> error "something went wrong"

-- procedure statement
procedureBodyEval:: ProcedureBody -> StateID Value
procedureBodyEval (Body stats ret) = do
                              statsEval stats
                              returnEval ret

-- procedure arguments, changes only the environment
argumentEval :: Arguments -> [Integer] -> StateID ()
argumentEval (Arg (Var name)) [i] = do
                      env <- get
                      put $ Map.insert name (Left i) env
argumentEval (Arg (Var name)) _ = error "number of arguments not equal to number of inputs"
argumentEval (Args (Var name) as) (i:is) = do
                      env <- get
                      put $ Map.insert name (Left i) env
                      argumentEval as is

{-| --------------------------
       auxiliary functions
-}  --------------------------

-- procedure arguments
argsToList:: Arguments -> [Var]
argsToList (Arg v) = [v]
argsToList (Args v vs) = v:(argsToList vs)

genRun :: (b -> StateID a) -> b -> Env -> a
genRun eval input env = runIdentity $ evalStateT (eval input) env

{-| --------------------------
       Extension 3.1: Procedure Calls
-}  --------------------------

argsListToExp:: ArgList -> [Expression]
argsListToExp (ArgI ex) = [ex]
argsListToExp (ArgsI ex exs) = ex:(argsListToExp exs)

evalExpressions:: Env -> [Expression] -> [Value]
evalExpressions env exps = map (\x -> genRun expEval x env) exps

valuesToInts :: [Value] -> [Integer]
valuesToInts vals = map strip vals
              where strip (Left i) = i
                    strip (Right expr) = error "TODO"

procedureEval :: [Integer] -> Procedure -> StateID Value
procedureEval inputs (Proc name args body) = do
                                  argumentEval args inputs
                                  procedureBodyEval body

proceduresEval :: Procedures -> StateID ()
proceduresEval Nil = return ()
proceduresEval (Procs p@(Proc (Var name) _ _) ps) = do
                                        env <- get
                                        put $ Map.insert name (Right p) env
                                        proceduresEval ps

mainEval :: [Integer] -> Main -> StateID Value
mainEval inputs (Main args body) = do
                      argumentEval args inputs
                      procedureBodyEval body

evalProgram :: [Integer] -> Program -> StateID Value
evalProgram inputs (Prog main procs) = do
                              proceduresEval procs
                              mainEval inputs main

runProgram :: [Integer] -> Program -> Value
runProgram inputs p = genRun (evalProgram inputs) p Map.empty

restoreEnv :: Env -> Arguments -> Env
restoreEnv env (Arg (Var n)) = case Map.lookup n env of
                                      Nothing -> env
                                      (Just val) ->  Map.insert n val env
restoreEnv env (Args (Var n) as) = case Map.lookup n env of
                                      Nothing -> restoreEnv env as
                                      (Just val) ->  Map.insert n val (restoreEnv env as)
