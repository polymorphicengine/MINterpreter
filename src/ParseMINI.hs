{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Parser
Description : Parses a MINI program to an abstract syntax tree
Copyright   : (c) Markus Rinke, 2021
                  Martin Gius, 2021
License     : GPL-3
Maintainer  : martin.gius@tuwien.ac.at
Stability   : experimental

-}
module ParseMINI where

import GHC.Generics
import Text.Parsec.String(Parser)
import Text.Parsec --(many, satisfy, try, (<|>), parse, (<?>))
import Text.Parsec.Char(char, oneOf, digit, string)
import Text.Parsec.Combinator(choice, eof, between, many1)
import Text.Parsec.Error
import Data.Char (isLetter, isDigit)
import Control.Monad(void)

{-  ------------------------
           terminals
-}  ------------------------

data Relator = EQQ | NEQ | LEQ | GEQ | LE | GE deriving (Show, Eq, Generic)
data Operator = Plus | Minus | Times | Divide deriving (Show, Eq, Generic)
-- data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Enum)
-- data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq)

{-  ------------------------
          non-Terminals
-}  ------------------------

-- data Ident_Char = Let Letter | Dig Int | Score deriving (Show, Eq)
-- data Ident_Rest = Eps | Rest Ident_Char Ident_Rest deriving (Show, Eq)
-- data Ident = Ident Letter Ident_Rest deriving (Show, Eq)
-- newtype Ident = Ident String deriving (Show, Eq)

type Name = String
-- | variables are typesafe strings
newtype Var = Var Name deriving (Show,Eq,Generic)
-- | digits are modelled as integers
data ExpressionNested = ECall Call | ENum Integer | EVar Var | Exp Expression deriving (Show,Eq,Generic)
data Expression = Pos ExpressionNested | Neg ExpressionNested | Term ExpressionNested Operator ExpressionNested deriving (Show,Eq,Generic)
newtype Return = Return Var deriving (Show,Eq,Generic)
data Boolean = BExp Expression Relator Expression deriving (Show,Eq,Generic)
data Assign = Ass Var Expression deriving (Show,Eq,Generic)
data If = If Boolean Statements | Elif Boolean Statements Statements deriving (Show,Eq,Generic)
data While = While Boolean Statements deriving (Show,Eq,Generic)
data Statement = WSt While | ISt If | ASt Assign | RSt ReadSt | PSt Print deriving (Show,Eq,Generic)
data Statements = Eps | St Statement Statements deriving (Show,Eq,Generic)
data ProcedureBody = Body Statements Return deriving (Show,Eq,Generic)
data Arguments = Arg Var | Args Var Arguments deriving (Show,Eq,Generic)
data Main = Main Arguments ProcedureBody deriving (Show,Eq,Generic)


{-  --------------------------
       auxiliary functions
-}  --------------------------

isSmallLetter :: Char -> Bool
isSmallLetter c = elem c "abcdefghijklmnopqrstuvwxyz"

isOperator:: Char -> Bool
isOperator c = elem c "+-*/"

-- | parse (>= 1) ASCII digits, return parsed characters
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

-- | consuming whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- | consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- | parse single character, return parsed character
symbol :: Char -> Parser Char
symbol c = lexeme $ char c

-- | parse "(", parse ")", return enclosed characters
betweenParens :: Parser a -> Parser a
betweenParens p = between (symbol '(') (symbol ')') p

-- | parse "{", parse "}", return enclosed characters
betweenParensCurly :: Parser a -> Parser a
betweenParensCurly p = between (symbol '{') (symbol '}') p

catch:: Either ParseError a -> a
catch (Left err) = error (show err)
catch (Right expr) = expr

{-  --------------------------
         symbol parsers
-}  --------------------------

operatorParse :: Parser Operator
operatorParse = do
      x <- (oneOf "+-*/")
      return $ chooseOp x
           where chooseOp '+' = Plus
                 chooseOp '-' = Minus
                 chooseOp '*' = Times
                 chooseOp '/' = Divide

geqP :: Parser Relator
geqP = do
     x <- string ">="
     return GEQ

leqP :: Parser Relator
leqP = do
     x <- string "<="
     return LEQ

eqP :: Parser Relator
eqP = do
     x <- string "=="
     return EQQ

neqP :: Parser Relator
neqP = do
     x <- string "!="
     return NEQ

leP :: Parser Relator
leP = do
     x <- string "<"
     return LE

geP :: Parser Relator
geP = do
     x <- string ">"
     return GE

{-  --------------------------------------
         well-formed-formula parsers
-}  --------------------------------------

-- | parses numerical expressions
numE :: Parser ExpressionNested
numE = do
  x <- lexeme num
  return $ ENum x

-- | parses variable expressions
variable :: Parser Var
variable = do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isSmallLetter a)
    nonFirstChar = satisfy (\a -> isDigit a || isSmallLetter a || a == '_')

-- | parses nested variable expressions
varE :: Parser ExpressionNested
varE = do
  x <- lexeme variable
  return $ EVar x

-- | parses postive expressions
expPos :: Parser Expression
expPos = do
      x <- lexeme expNestParse
      return $ Pos x

-- | parses negative expressions
expNeg :: Parser Expression
expNeg = do
      fc <- char '-'
      x <- lexeme expNestParse
      return $ Neg x

-- | parses combined expressions
expTerm:: Parser Expression
expTerm = do
      x <- lexeme expNestParse
      o <- lexeme operatorParse
      y <- lexeme expNestParse
      return $ Term x o y

-- | parses bracketed expression
bracesE :: Parser ExpressionNested
bracesE = do
      e <- lexeme $ betweenParens expParse
      return $ Exp e

-- | parses boolean expressions
booleanParse :: Parser Boolean
booleanParse = do
            x <- lexeme expParse
            r <- lexeme relatorParse
            y <- lexeme expParse
            return $ BExp x r y

-- combine parsers to parse well-formed-expressions

-- | combined nested expression parser
expNestParse :: Parser ExpressionNested
expNestParse = lexeme bracesE <|> (try $ lexeme callE) <|> lexeme varE <|> lexeme numE

-- | combined relator parser
relatorParse :: Parser Relator
relatorParse = try geqP <|> try leqP <|> eqP <|> neqP <|> leP <|> geP

-- | combined expression parser
-- first tries to parse as combined expressions, if that fails, it tries parsing it as a negative or positive expression
expParse :: Parser Expression
expParse = choice [try $ lexeme expTerm, lexeme expNeg, lexeme expPos]

{-  --------------------------
           statements
-}  --------------------------

-- | parses variable assignment
assignParse :: Parser Assign
assignParse = do
           x <- lexeme $ variable
           y <- lexeme $ char '='
           z <- lexeme $ expParse
           semi <- lexeme $ char ';'
           return $ Ass x z

-- | parses if statements
ifParse' :: Parser If
ifParse' = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ If y z

-- | parses  if-else statements
elifParse :: Parser If
elifParse = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      el <- lexeme $ string "else"
      elS <- lexeme $ betweenParensCurly statementsParse
      return $ Elif y z elS

-- | combined parser for if statements
ifParse :: Parser If
ifParse = try elifParse <|> ifParse'

-- | parses while statements
whileParse :: Parser While
whileParse = do
      x <- lexeme $ string "while"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ While y z

-- | parses an empty statement
emptyParse :: Parser Statements
emptyParse = return Eps

-- | combined parser for statements
-- it first tries while, if, read and print parsers, if those fail, lastly the assign parser is called
statementParse :: Parser Statement
statementParse = (try $ fmap WSt whileParse) <|> (try $ fmap ISt ifParse) <|> (try $ fmap RSt readParse) <|> (try $ fmap PSt printParse) <|> fmap ASt assignParse

-- | recursively parse statements
statementsParseRec :: Parser Statements
statementsParseRec = do
            x <- lexeme statementParse
            y <- lexeme statementsParse
            return $ St x y

-- | if the recursive parser fails, call the empty parser
statementsParse :: Parser Statements
statementsParse = try statementsParseRec <|> emptyParse

{-  --------------------------
            program
-}  --------------------------

-- | parse return statements
returnParse :: Parser Return
returnParse = do
      x <- lexeme $ string "return"
      v <- lexeme $ variable
      semi <- lexeme $ char ';'
      return $ Return v

-- | parse the procedure body
procBodyParse :: Parser ProcedureBody
procBodyParse = do
      stat <- lexeme $ statementsParse
      ret <- lexeme $ returnParse
      return $ Body stat ret

-- | parse the procedure arguments recursively
argVarParseRec :: Parser Arguments
argVarParseRec = do
            x <- lexeme $ variable
            col <- lexeme $ char ','
            xs <- lexeme $ argVarParse
            return $ Args x xs

-- | parse the procedure arguments
argVarParse :: Parser Arguments
argVarParse = try argVarParseRec <|> (fmap Arg variable)

-- | parse the main procedure
mainParse :: Parser Main
mainParse = do
      key <- lexeme $ string "procedure main"
      args <- lexeme $ betweenParens argVarParse
      body <- lexeme $ betweenParensCurly procBodyParse
      return $ Main args body

mainParseEOF :: Parser Main
mainParseEOF = do
        whitespace
        prog <- lexeme $ mainParse
        eof
        return prog

{- ------------------------------------------
          Extension 3.1: Procedure Calls
-}  ------------------------------------------

data Program = Prog Main Procedures deriving (Show,Eq,Generic)
data Procedures = Nil | Procs Procedure Procedures deriving (Show,Eq,Generic)
data Procedure = Proc Var Arguments ProcedureBody deriving (Show,Eq,Generic)
data Call = Call Var ArgList deriving (Show,Eq,Generic)
data ArgList = ArgI Expression | ArgsI Expression ArgList deriving (Show,Eq,Generic)

-- | parse call expressions
callE :: Parser ExpressionNested
callE = do
      ident <- lexeme variable
      args <- lexeme $ betweenParens argListParse
      return $ ECall (Call ident args)

-- | recursively parse arguments to procedure calls
argListParseRec :: Parser ArgList
argListParseRec = do
            x <- lexeme expParse
            col <- lexeme $ char ','
            xs <- lexeme $ argListParse
            return $ ArgsI x xs

-- | parse arguments to procedure calls
argListParse :: Parser ArgList
argListParse = try argListParseRec <|> (fmap ArgI expParse)

-- | parse procedures, analogously to main procedures
procParse :: Parser Procedure
procParse = do
  key <- lexeme $ string "procedure"
  ident <- lexeme variable
  argVars <- lexeme $ betweenParens argVarParse
  body <- lexeme $ betweenParensCurly procBodyParse
  return $ Proc ident argVars body

-- | recursively parse procedures
procsParseRec :: Parser Procedures
procsParseRec = do
            x <- lexeme procParse
            y <- lexeme procsParse
            return $ Procs x y

-- | if the recursive parser fails, return the empty procedure
procsParse :: Parser Procedures
procsParse = try procsParseRec <|> (return Nil)

-- | parse programs
programParse :: Parser Program
programParse = do
        main <- lexeme mainParse
        procs <- lexeme procsParse
        return $ Prog main procs

-- | parse programs - fails if after the program is no whitespace
programParseEOF :: Parser Program
programParseEOF = do
        whitespace
        prog <- lexeme $ programParse
        eof
        return prog


{- ---------------------------------
          Extension 3.2: IO
-}  ---------------------------------

newtype Print = Print Expression deriving (Show,Eq,Generic)
newtype ReadSt = Read Var deriving (Show,Eq,Generic)

-- | parse print statements
printParse :: Parser Print
printParse = do
      string <- lexeme $ string "print_int"
      expr <- lexeme $ betweenParens expParse
      sem <- lexeme $ char ';'
      return $ Print expr

-- | parse read statements
readParse :: Parser ReadSt
readParse = do
      var <- lexeme $ variable
      eq <- lexeme $ char '='
      string <- lexeme $ string "read_int();"
      return $ Read var


{- --------------------------------
          Extension 3.4: Erorrs
-}  --------------------------------

{-

data Type = E00 | E01 | E02 | E03 | E04 | E05

instance Show Type where
  show E00 = "E00: Unknown error"
  show E01 = "E01: mismatched parantheses"
  show E02 = "E02: unknown keyword"
  show E03 = "E03: missing semicolon"
  show E04 = "E04: invalid identifier"
  show E05 = "E05: missing return statement at the end of procedure"

data PError = PError {typ :: Type, pos:: SourcePos}

instance Show PError where
  show (PError t pos) = "The following error occured at " ++ show pos ++ ":" ++ "\n" ++ show t

transformError :: ParseError -> PError
transformError p = PError typ (errorPos p)
            where msgs = errorMessages p
                  typ = getType (transformMessages msgs)

transformMessages :: [Message] -> [String]
transformMessages [] = []
transformMessages ((Expect s):ms) = s:(transformMessages ms)
transformMessages (m:ms) = (transformMessages ms)

getType :: [String] -> Type
getType [] = E00
getType (x:_) = case x of
                  "E01" -> E01
                  "E02" -> E02
                  "E03" -> E03
                  "E04" -> E04
                  "E05" -> E05
                  _ -> E00

parse' ::  Parser a -> SourceName -> String -> Either PError a
parse' p s i = case parse p s i of
                      (Left err) -> Left $ transformError err
                      (Right x) -> Right x
-}
