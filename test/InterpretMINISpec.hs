module InterpretMINISpec where

import ParseMINI
import InterpretMINI
import Test.Hspec
import Control.Monad.Identity
import Control.Monad.State
import Control.Exception
import Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "evaluation of number expressions" $ do
      it "evaluates expressions without variables" $ do
        (genRun expEval (Term (Exp (Term (ENum 1) Plus (ENum 3))) Times (ENum 4)) Map.empty) `shouldBe` 16
      it "evaluates expressions with defined variables" $ do
        (genRun expEval (Term (Exp (Term (EVar (Var "x")) Plus (ENum 3))) Times (ENum 4)) (Map.insert "x" 4 Map.empty) ) `shouldBe` 28
      it "it fails on undefined variable calls" $ do
        (evaluate (genRun expEval (Term (Exp (Term (EVar $ Var "abc") Plus (ENum 3))) Times (ENum 4)) Map.empty)) `shouldThrow` anyException
      it "it fails on division by zero" $ do
        (evaluate (genRun expEval (Term (Exp (Term (ENum 3) Divide (ENum 0))) Times (ENum 4)) Map.empty)) `shouldThrow` anyException
