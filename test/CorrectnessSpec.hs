module CorrectnessSpec where

  import Correctness(prog1_corr,prog2_corr,prog3_corr)
  import Test.Hspec
  import Test.Hspec.QuickCheck

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    prop "checks the correctness of the fibonacci program" $ prog1_corr
    prop "checks the correctness of the prime number check" $ prog2_corr
    prop "checks the correctness of the lcm program" $ prog3_corr
