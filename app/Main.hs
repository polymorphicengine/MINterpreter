{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import ParseMINI(programParseEOF, catch)
import InterpretMINI(runProgram)
import Formatting
import Cli(execCli, Cli (Pretty, Exec, Check, Ext, Logo))
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
mainChoice (Check _ path) = do
                    miniProg <- readFile path
                    let parsed = (parse programParseEOF "" miniProg)
                    case parsed of
                      (Right p) -> print "Your MINI program does not have syntax errors!"
                      (Left err) -> print err
mainChoice (Ext _) = putStrLn "The available MINI extensions are: \n Named Procedures \n IO System \n Source Code Formatting"
mainChoice (Logo _) = putStrLn "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@===========@@@@@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@=-----------=@@@@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@=------------=@@@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@=======-------=@@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@@--------=@@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@@@--------=@@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@@@=--------=@@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@@@--------- @@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@=------------@@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@---------------@@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@=-----------------@@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@---------@@=--------@@@@@@@@@@@@@@@ \n @@@@@@@@@@@@@=--------=@@@@=--------@@@@@@@@@@@@@@ \n @@@@@@@@@@@=--------=@@@@@@@=--------@@@@@@@@@@@@@ \n @@@@@@@@@@=--------=@@@@@@@@@=--------@@@@@@@@@@@@ \n @@@@@@@@=--------=@@@@@@@@@@@@=-------=@@@@===@@@@ \n @@@@@@@=--------@@@@@@@@@@@@@@@=--------------=@@@ \n @@@@@=--------=@@@@@@@@@@@@@@@@@---------------@@@ \n @@@@=-=======@@@@@@@@@@@@@@@@@@@@--------====@@@@@ \n @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@===@@@@@@@@@@@@@ \n @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
