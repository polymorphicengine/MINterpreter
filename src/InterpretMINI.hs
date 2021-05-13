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

-- evalExpNest' :: Env -> ExpressionNested -> Value
-- evalExpNest' env (ENum i) = i
-- evalExpNest' env (EVar $ Var name) = Map.lookup name env
-- evalExpNest' env (Exp expr) = evalExp' env expr
--
-- evalExp' :: Env -> Expression -> Value
-- evalExp' env (Pos expr) = evalExpNest' env expr
-- evalExp' env (Neg expr) = - (evalExpNest' env expr)
-- evalExp' env (Term exp1 op exp2) = (evalExpNest' exp1) (getOp op) (evalExpNest' exp2)
--                             where getOp Plus = (+)
--                                   getOp Minus = (-)
--                                   getOp Times = (*)
--                                   getOp Divide = (/)

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

-- putt :: s -> State s ()
-- putt newState = state $ \_ -> ((), newState)

assignEval :: Env -> Assign -> StateID ()
assignEval env (Ass (Var name) expr) = do
                                  num <- evalExp env expr
                                  put newEnv
                                  return ()
                                  where newEnv = Map.insert name 4 env

-- runIdentity $(evalStateT $ evalExp Map.empty (Pos (ENum 5))) Map.empty
-- runIdentity $(evalStateT $ evalExp Map.empty ( (Term (Exp (Term (ENum 1) Plus (ENum 2))) Times (ENum 3)) )) Map.empty
