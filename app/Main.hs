module Main where

import ParseMINI
import InterpretMINI
import Text.Parsec
import System.Environment(getArgs)
import Control.Monad.State


main :: IO ()
main = do
  args <- getArgs
  let path = args!!0
  let inputs = map read (tail args) :: [Integer]
  miniProg <- readFile path
  let prog = catch (parse programParseEOF "" miniProg)
  let output = runProgram inputs prog
  out <- fmap strip output
  print out
  where strip (Left i) = i
        strip (Right expr) = error "think again ;) cannot return procedures"
