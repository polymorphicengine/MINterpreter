module Main where

import ParseMINI
import InterpretMINI
import Text.Parsec
import System.Environment(getArgs)


main :: IO ()
main = do
  args <- getArgs
  let path = args!!0
  let inputs = map read (tail args) :: [Integer]
  miniProg <- readFile path
  let prog = catch (parse programParseEOF "" miniProg)
  let output = runProgram inputs prog
  print output
