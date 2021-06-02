{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import ParseMINI(programParseEOF, catch)
import InterpretMINI(runProgram)
import Formatting
import Cli(execCli, Cli (Pretty, Exec))
import Text.Parsec(parse)
import Control.Monad.State
import Prettyprinter(pretty)


main :: IO ()
main = do
  x <- execCli
  mainChoice x

mainChoice :: Cli -> IO ()
mainChoice (Pretty _ path) = do
                    miniProg <- readFile path
                    let prog = catch (parse programParseEOF "" miniProg)
                    print (pretty prog)
mainChoice (Exec path args) = do
                  let inputs = map read args :: [Integer]
                  miniProg <- readFile path
                  let prog = catch (parse programParseEOF "" miniProg)
                  let output = runProgram inputs prog
                  out <- fmap strip output
                  print out
                  where strip (Left i) = i
                        strip (Right expr) = error "think again ;) cannot return procedures"
