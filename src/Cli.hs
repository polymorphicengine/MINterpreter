module Cli where

import Options.Applicative

data Mode = Format | Normal deriving(Show, Eq)

data Cli = Pretty Mode FilePath | Exec FilePath [String] deriving(Show, Eq)

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

parser :: Parser Cli
parser =  parserExec <|> parserPretty

execCli :: IO Cli
execCli = do
  x <- execParser $ (info $ parser <**> helper) (fullDesc
     <> progDesc "Either execute or format a MINI program"
     <> header "MINterpreter - an interpreter with pretty print capabilities" )
  return x
