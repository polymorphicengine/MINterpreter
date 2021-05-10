module Parser where

import Text.Parsec.String(Parser)
import Text.Parsec(parse,try)
import Text.Parsec.Char (oneOf, char, digit, satisfy)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Text.Parsec.Error
import Data.Char (isLetter, isDigit)
import Control.Monad(void)
import Control.Applicative ((<|>), many)

--Terminals

data Relator = EQ | NEQ | LEQ | GEQ | LE | GE deriving (Show, Eq)
data Operator = Plus | Minus | Times | Divide deriving (Show, Eq)
-- data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Enum)
-- data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq)

--Non-Terminals

-- data Ident_Char = Let Letter | Dig Int | Score deriving (Show, Eq)
-- data Ident_Rest = Eps | Rest Ident_Char Ident_Rest deriving (Show, Eq)
-- data Ident = Ident Letter Ident_Rest deriving (Show, Eq)
-- newtype Ident = Ident String deriving (Show, Eq)
newtype Var = Var String deriving (Show, Eq)
-- data Number = Digit Digit | Number Digit Number deriving (Show, Eq)
data ExpressionNested = ENum Integer | EVar Var | Exp Expression deriving (Show, Eq)
data Expression = Pos ExpressionNested | Neg ExpressionNested | Term ExpressionNested Operator ExpressionNested deriving (Show, Eq)
newtype Return = Return Var deriving (Show, Eq)
data Boolean = BExp Expression Relator Expression deriving (Show, Eq)
data Assign = Ass Var Expression deriving (Show, Eq)
data If = If Boolean Statement | Elif Boolean Statement Statement deriving (Show, Eq)
data While = While Boolean Statement deriving (Show, Eq)
data Statement = WSt While | ISt If | ASt Assign deriving (Show, Eq)
data Statements = Empty | St Statement Statements deriving (Show, Eq)
data Procedure = Proc Statements Return deriving (Show, Eq)
data Arguments = Arg Var | Args Var Arguments deriving (Show, Eq)
data Program = Prog Arguments Procedure deriving (Show, Eq)

-- "1/(2+(-3))"

-- digitParser :: Parser Digit

-- variAbLE44

-- fx_55_asd9
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

isSmallLetter :: Char -> Bool
isSmallLetter c = elem c "abcdefghijklmnopqrstuvwxyz"

isOperator:: Char -> Bool
isOperator c = elem c "+-/*"

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

numE :: Parser ExpressionNested
numE = do
  x <- lexeme num
  return $ ENum x

variable :: Parser Var
variable = do
    fc <- firstChar
    rest <- many1 nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isSmallLetter a)
    nonFirstChar = satisfy (\a -> isDigit a || isSmallLetter a || a == '_')

varE :: Parser ExpressionNested
varE = do
  x <- lexeme variable
  return $ EVar x

expPos :: Parser Expression
expPos = do
      x <- lexeme expNestParse
      return $ Pos x

expNeg :: Parser Expression
expNeg = do
      fc <- char '-'
      x <- lexeme expNestParse
      return $ Neg x

operatorParse :: Parser Operator
operatorParse = do
      x <- (oneOf "+-*/")
      return $ chooseOp x
           where chooseOp '+' = Plus
                 chooseOp '-' = Minus
                 chooseOp '*' = Times
                 chooseOp '/' = Divide

expTerm:: Parser Expression
expTerm = do
      x <- lexeme expNestParse
      o <- lexeme operatorParse
      y <- expNestParse
      return $ Term x o y

bracesE :: Parser ExpressionNested
bracesE = do
    void $ lexeme $ char '('
    e <- expParse
    void $ lexeme $ char ')'
    return $ Exp e

expParse :: Parser Expression
expParse = choice [try expTerm, expPos] <|>  expNeg

expNestParse :: Parser ExpressionNested
expNestParse = bracesE <|> varE <|> numE

catch:: Either ParseError Expression -> String
catch (Left err) = show err
catch (Right expr) = show expr

--test
--Test: "1 +(3 + 4)"
