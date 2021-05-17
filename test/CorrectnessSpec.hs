module CorrectnessSpec where

  import Correctness(prog1_corr)
  import Test.Hspec
  import Test.Hspec.QuickCheck

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do prop "checks the correctness of the fibonacci program" $ prog1_corr
