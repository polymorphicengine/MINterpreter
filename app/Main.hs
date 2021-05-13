module Main where

import ParseMINI
import InterpretMINI
import Text.Parsec

main :: IO ()
main =  do
  putStrLn "Enter a mini program:"
  x <- getLine
  putStrLn $ catch $ parse programParseEOF "" x
