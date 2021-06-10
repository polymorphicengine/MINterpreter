{-|
Module      : CLI
Description : A command line interface
Copyright   : (c) Markus Rinke, 2021
                  Martin Gius, 2021
License     : GPL-3
Maintainer  : martin.gius@tuwien.ac.at
Stability   : experimental

-}
module Cli where

import Options.Applicative

data Mode = Format | Normal deriving(Show, Eq)

data Cli = Pretty Mode FilePath | Exec FilePath [String] | Check Bool FilePath | Ext Bool | Logo Bool deriving(Show, Eq)

parserLogoBool :: Parser Bool
parserLogoBool = flag True False
  ( long "logo"
 <> short 'l'
 <> help "Print the logo of the MINterpreter" )

parserExtensionsBool :: Parser Bool
parserExtensionsBool = flag True False
  ( long "extensions"
 <> short 'e'
 <> help "Print all available MINI extensions" )

parserCheckBool :: Parser Bool
parserCheckBool = flag True False
  ( long "check"
 <> short 'c'
 <> help "Check a program for syntax errors" )

parserMode :: Parser Mode
parserMode = flag Normal Format
  ( long "format"
 <> short 'f'
 <> help "Enable formatting mode" )

parserPath :: Parser FilePath
parserPath = argument str (metavar "FILE_PATH")

parserArgs :: Parser [String]
parserArgs = (some $ argument str (metavar "ARGS"))

parserPretty :: Parser Cli
parserPretty = Pretty <$> parserMode <*> parserPath

parserExec :: Parser Cli
parserExec = Exec <$>  parserPath <*> parserArgs

parserCheck :: Parser Cli
parserCheck = Check <$> parserCheckBool <*> parserPath

parserExtensions :: Parser Cli
parserExtensions = Ext <$> parserExtensionsBool

parserLogo :: Parser Cli
parserLogo = Logo <$> parserLogoBool

parser :: Parser Cli
parser =  parserExec <|> parserPretty <|> parserCheck <|> parserExtensions <|> parserLogo

execCli :: IO Cli
execCli = do
  x <- execParser $ (info $ parser <**> helper) (fullDesc
     <> progDesc "Execute, format or check the syntax of a MINI program"
     <> header "MINterpreter - an interpreter with pretty print functionality" )
  return x
