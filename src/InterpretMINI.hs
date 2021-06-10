module InterpretMINI where

import ParseMINI
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

-- | values can be either integers or procedures
-- since procedure calls are also recognized through identifiers
type Value = Either Integer Procedure
-- | An environment assigns to each name a value
type Env = Map.Map Name Value
-- | the stateIO monad is a combination of the IO monad and the State monad
-- this is achieved by means of the StateT monad transformer
type StateIO = StateT Env IO -- handling states

{- ---------------------------------------
        well-formed-expression interpreter
-}  ---------------------------------------


-- | evaluates nested expression
-- to evaluate a call expression, first the arguments have to be evaluated, and the corresponding procedure has to be looked up in the environment and evaluated with the arguments
-- to evaluate a variable, its value is looked up in the environemt
expNestEval:: ExpressionNested -> StateIO Value
expNestEval (ECall (Call (Var name) argList)) = do
                          env <- get
                          case Map.lookup name env of
                                    Nothing -> error "undefined procedure call"
                                    (Just (Left val) ) -> error "something went wrong"
                                    (Just (Right p@(Proc _ args _))) -> do
                                                                  vals <- evalExpressions (argsListToExp argList)
                                                                  let inputs = map strip vals
                                                                  val <- procedureEval inputs p
                                                                  put env   -- the original environment is restored
                                                                  return val
                                                                  where strip (Left i) = i
expNestEval (ENum i) = return (Left i)
expNestEval (EVar (Var name)) = do
                          env <- get
                          case Map.lookup name env of
                                    Nothing -> error "undefined variable call"
                                    (Just (Left val) ) -> return (Left val)
                                    (Just (Right procedure)) -> error "something went wrong"
expNestEval (Exp expr) = expEval expr

-- | evaluates combined expressions
expEval :: Expression -> StateIO Value
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

-- | evaluates boolean expressions
boolEval :: Boolean -> StateIO Bool
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

{- ----------------------------------------
            statement interpreter
-}  ---------------------------------------

-- | evaluates a variable assignment
-- by evaluating the expression and updating the environment
assignEval :: Assign -> StateIO ()
assignEval (Ass (Var name) expr) = do
                            env <- get
                            num <- expEval expr
                            put (Map.insert name num env)

-- | evaluate if and if-else statements
ifEval:: If -> StateIO ()
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

-- | evaluates while statements
-- by evaluating the inner statements once if the boolean expression evaluates to true and calls itself again
whileEval:: While -> StateIO ()
whileEval w@(While boolExp stats) = do
                      bool <- boolEval boolExp
                      if bool then
                        do
                          statsEval stats
                          whileEval w
                      else
                        return ()

-- | evaluates statements by combining the previous evaluation functions
statEval:: Statement -> StateIO ()
statEval (WSt w) = whileEval w
statEval (ISt i) = ifEval i
statEval (ASt a) = assignEval a
statEval (RSt r) = readEval r
statEval (PSt p) = printEval p

-- | evaluates statements recursively
statsEval :: Statements -> StateIO ()
statsEval Eps = return ()
statsEval (St stat stats) = do
                        statEval stat
                        statsEval stats

{- ---------------------------
            program
-}  --------------------------

-- | evaluates the return statement by looking up the name in the environment
returnEval :: Return -> StateIO Value
returnEval (Return (Var name)) = do
                          env <- get
                          case Map.lookup name env of
                                Nothing -> error "undefined variable call"
                                (Just (Left val) ) -> return (Left val)
                                (Just (Right procedure)) -> error "something went wrong"

-- | evaluates the procedure body
procedureBodyEval:: ProcedureBody -> StateIO Value
procedureBodyEval (Body stats ret) = do
                              statsEval stats
                              returnEval ret

-- | evaluates the procedure arguments by updating the environment
argumentEval :: Arguments -> [Integer] -> StateIO ()
argumentEval (Arg (Var name)) [i] = do
                      env <- get
                      put $ Map.insert name (Left i) env
argumentEval (Arg (Var name)) _ = error "number of arguments not equal to number of inputs"
argumentEval (Args (Var name) as) (i:is) = do
                      env <- get
                      put $ Map.insert name (Left i) env
                      argumentEval as is

{- ---------------------------
       auxiliary functions
-}  --------------------------


argsToList:: Arguments -> [Var]
argsToList (Arg v) = [v]
argsToList (Args v vs) = v:(argsToList vs)

-- | generic implementation for running the StateIO monad
genRun :: (b -> StateIO a) -> b -> Env -> IO a
genRun eval input env = evalStateT (eval input) env

{- --------------------------------------
       Extension 3.1: Procedure Calls
-}  -------------------------------------

argsListToExp:: ArgList -> [Expression]
argsListToExp (ArgI ex) = [ex]
argsListToExp (ArgsI ex exs) = ex:(argsListToExp exs)

evalExpressions:: [Expression] -> StateIO [Value]
evalExpressions [expr] = do
                        ev <- expEval expr
                        return [ev]
evalExpressions (expr:exprs) = do
                        ev1 <- expEval expr
                        evs <- evalExpressions exprs
                        return $ ev1:evs

-- | evaluates a procedure with given arguments
procedureEval :: [Integer] -> Procedure -> StateIO Value
procedureEval inputs (Proc name args body) = do
                                  argumentEval args inputs
                                  procedureBodyEval body

-- | writes procedures to the environment
proceduresEval :: Procedures -> StateIO ()
proceduresEval Nil = return ()
proceduresEval (Procs p@(Proc (Var name) _ _) ps) = do
                                        env <- get
                                        put $ Map.insert name (Right p) env
                                        proceduresEval ps

-- | evaluates a main procedure on a given input
mainEval :: [Integer] -> Main -> StateIO Value
mainEval inputs (Main args body) = do
                      argumentEval args inputs
                      procedureBodyEval body

-- | evaluates a program on a given input
evalProgram :: [Integer] -> Program -> StateIO Value
evalProgram inputs (Prog main procs) = do
                              proceduresEval procs
                              mainEval inputs main

-- | runs the program on given inputs
runProgram :: [Integer] -> Program -> IO Value
runProgram inputs p = genRun (evalProgram inputs) p Map.empty


{- -----------------------------
          Extension 3.1: IO
-}  ----------------------------

-- | evaluates read expressions
readEval :: ReadSt -> StateIO ()
readEval (Read (Var name)) = do
                      env <- get
                      liftIO $ putStrLn "Enter a value: \n"
                      input <- liftIO getLine
                      let number = read input :: Integer
                      put $ Map.insert name (Left number) env

-- | evaluates print expressions
printEval :: Print -> StateIO ()
printEval (Print expr) = do
                      ev <- expEval expr
                      liftIO $ putStr "MINI print: "
                      liftIO $ print (strip ev)
                      liftIO $ putChar '\n'
                      where strip (Left i) = i
