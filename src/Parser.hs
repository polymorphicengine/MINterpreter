module Parser where

import Text.Parsec.String
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Data.Char (isLetter, isDigit)
import Control.Monad(void)
-- import Control.Applicative ((<|>))

--Terminals

data Relator = EQQ | NEQ | LEQ | GEQ | LE | GE deriving (Show, Eq)
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
data If = If Boolean Statements | Elif Boolean Statements Statements deriving (Show, Eq)
data While = While Boolean Statements deriving (Show, Eq)
data Statement = WSt While | ISt If | ASt Assign deriving (Show, Eq)
data Statements = Eps | St Statement Statements deriving (Show, Eq)
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
      y <- lexeme expNestParse
      return $ Term x o y

bracesE :: Parser ExpressionNested
bracesE = do
    void $ lexeme $ char '('
    e <- expParse
    void $ lexeme $ char ')'
    return $ Exp e

expParse :: Parser Expression
expParse = choice [try $ lexeme expTerm, lexeme expNeg, lexeme expPos]

expParseWithEOF :: Parser Expression
expParseWithEOF = do
                whitespace
                x <- lexeme expParse
                eof
                return x

expNestParse :: Parser ExpressionNested
expNestParse = lexeme bracesE <|> lexeme varE <|> lexeme numE


catch:: Either ParseError Expression -> String
catch (Left err) = show err
catch (Right expr) = show expr

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

relatorParse :: Parser Relator
relatorParse = try geqP <|> try leqP <|> eqP <|> neqP <|> leP <|> geP

booleanParse :: Parser Boolean
booleanParse = do
            x <- lexeme expParse
            r <- lexeme relatorParse
            y <- lexeme expParse
            return $ BExp x r y

assignParse :: Parser Assign
assignParse = do
            x <- lexeme $ variable
            y <- lexeme $ char '='
            z <- lexeme $ expParse
            semi <- lexeme $ char ';'
            return $ Ass x z

ifParse' :: Parser If
ifParse' = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ If y z

elifParse :: Parser If
elifParse = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      el <- lexeme $ string "else"
      elS <- lexeme $ betweenParensCurly statementsParse
      return $ Elif y z elS

ifParse :: Parser If
ifParse = try elifParse <|> ifParse'

whileParse :: Parser While
whileParse = do
      x <- lexeme $ string "while"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ While y z

statementsParseRec :: Parser Statements
statementsParseRec = do
            x <- lexeme statementParse
            y <- lexeme statementsParse
            return $ St x y

statementsParse :: Parser Statements
statementsParse = statementsParseRec <|> emptyParse

statementParse :: Parser Statement
statementParse = fmap WSt whileParse <|> fmap ISt ifParse <|> fmap ASt assignParse

symbol :: Char -> Parser Char
symbol c = lexeme $ char c

betweenParens :: Parser a -> Parser a
betweenParens p = between (symbol '(') (symbol ')') p

betweenParensCurly :: Parser a -> Parser a
betweenParensCurly p = between (symbol '{') (symbol '}') p

emptyParse :: Parser Statements
emptyParse = return Eps

returnParse :: Parser Return
returnParse = do
      x <- lexeme $ string "return"

--Test: "1 +(3 + 4)"

--- exp := var | int | -var | -int | (-exp) | (exp op exp)

-- lookAhead $ satisfy (\a -> a == '(') <|>
