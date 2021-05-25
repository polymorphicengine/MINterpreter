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
      it "evaluates expressions without variables" $ do
        (genRun expEval (Term (Exp (Term (Exp (Term (ENum 3) Minus (ENum 1))) Minus (Exp (Term (ENum 3) Minus (ENum 1))))) Minus (Exp (Term (ENum 3) Minus (Exp (Neg (ENum 1)))))) Map.empty) `shouldBe` -4
        (genRun expEval (Pos (Exp (Term (Exp (Term (Exp (Term (Exp (Term (ENum 2) Divide (ENum 2))) Times (Exp (Neg (ENum 2))))) Divide (ENum 2))) Times (ENum 2)))) Map.empty) `shouldBe` -2

    describe "evaluation of boolean expressions" $ do
      it "evaluates boolean expressions without variables" $ do
        (genRun boolEval (BExp (Pos (ENum 2)) LE (Pos (ENum 3))) Map.empty) `shouldBe` True
        (genRun boolEval (BExp (Term (Exp (Term (Exp (Term (ENum 2) Minus (Exp (Neg (ENum 1))))) Times (ENum 4))) Divide (ENum 3)) LE (Pos (ENum 5))) Map.empty) `shouldBe` True
        (genRun boolEval (BExp (Term (Exp (Term (Exp (Term (ENum 2) Minus (Exp (Neg (ENum 1))))) Times (ENum 4))) Divide (ENum 3)) EQQ (Pos (ENum 5))) Map.empty) `shouldBe` False
      it "evaluates boolean expressions with defined variables" $ do
        (genRun boolEval (BExp (Pos (EVar (Var "a"))) GEQ (Pos (ENum 3)))  (Map.insert "a" 4 Map.empty) ) `shouldBe` True
        (genRun boolEval (BExp (Pos (EVar (Var "a"))) GEQ (Pos (ENum 3)))  (Map.insert "a" 2 Map.empty) ) `shouldBe` False
        (genRun boolEval (BExp (Pos (Exp (Term (EVar (Var "a")) Plus (EVar (Var "b"))))) EQQ (Pos (ENum 3))) (Map.insert "a" (-1) $ Map.insert "b" 4 Map.empty) ) `shouldBe` True

    describe "evaluation of programs" $ do
      it "evaluates program that calculates the greatest common divisor for non-negative integers" $ do
         runProgram [42,56] (Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ASt (Ass (Var "r") (Pos (EVar (Var "b"))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) NEQ (Pos (ENum 0))) (St (WSt (While (BExp (Pos (EVar (Var "b"))) NEQ (Pos (ENum 0))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "b") (Term (EVar (Var "b")) Minus (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "a") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps))) Eps))) (St (ASt (Ass (Var "r") (Pos (EVar (Var "a"))))) Eps)))) Eps)) (Return (Var "r")))) `shouldBe` 14
         runProgram [17,56] (Prog (Args (Var "a") (Arg (Var "b"))) (Proc (St (ASt (Ass (Var "r") (Pos (EVar (Var "b"))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) NEQ (Pos (ENum 0))) (St (WSt (While (BExp (Pos (EVar (Var "b"))) NEQ (Pos (ENum 0))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "b") (Term (EVar (Var "b")) Minus (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "a") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps))) Eps))) (St (ASt (Ass (Var "r") (Pos (EVar (Var "a"))))) Eps)))) Eps)) (Return (Var "r")))) `shouldBe` 1
      it "evaluates program that calculates the factorial for non-negative n" $ do
         runProgram [10] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "i") (Pos (ENum 1)))) (St (ASt (Ass (Var "fac") (Pos (ENum 1)))) (St (ISt (Elif (BExp (Pos (EVar (Var "n"))) LE (Pos (ENum 0))) (St (ASt (Ass (Var "fac") (Pos (ENum 0)))) Eps) (St (WSt (While (BExp (Pos (EVar (Var "n"))) GEQ (Pos (EVar (Var "i")))) (St (ASt (Ass (Var "fac") (Term (EVar (Var "fac")) Times (EVar (Var "i"))))) (St (ASt (Ass (Var "i") (Term (EVar (Var "i")) Plus (ENum 1)))) Eps)))) Eps))) Eps))) (Return (Var "fac")))) `shouldBe` 3628800
      it "evaluates program that reverses a number n" $ do
         runProgram [123456789] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "reverse") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "rem") (Term (EVar (Var "n")) Minus (Exp (Term (ENum 10) Times (Exp (Term (EVar (Var "n")) Divide (ENum 10)))))))) (St (ASt (Ass (Var "reverse") (Pos (Exp (Term (Exp (Term (EVar (Var "reverse")) Times (ENum 10))) Plus (EVar (Var "rem"))))))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) Eps))))) Eps)) (Return (Var "reverse")))) `shouldBe` 987654321
      it "evaluates program that counts the number of digits a number n" $ do
         runProgram [71283719823791273] (Prog (Arg (Var "n")) (Proc (St (ASt (Ass (Var "counter") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) (St (ASt (Ass (Var "counter") (Term (EVar (Var "counter")) Plus (ENum 1)))) Eps)))) Eps)) (Return (Var "counter")))) `shouldBe` 17
