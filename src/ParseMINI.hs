module ParseMINI where

{-| ------------------------
            import
-}  ------------------------

import Text.Parsec.String(Parser)
import Text.Parsec(many, satisfy, try, (<|>), parse)
import Text.Parsec.Char(char, oneOf, digit, string)
import Text.Parsec.Combinator(choice, eof, between, many1)
import Text.Parsec.Error(ParseError)
import Data.Char (isLetter, isDigit)
import Control.Monad(void)

{-| ------------------------
           terminals
-}  ------------------------

data Relator = EQQ | NEQ | LEQ | GEQ | LE | GE deriving (Show, Eq)
data Operator = Plus | Minus | Times | Divide deriving (Show, Eq)
-- data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Enum)
-- data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq)

{-| ------------------------
          non-Terminals
-}  ------------------------

-- data Ident_Char = Let Letter | Dig Int | Score deriving (Show, Eq)
-- data Ident_Rest = Eps | Rest Ident_Char Ident_Rest deriving (Show, Eq)
-- data Ident = Ident Letter Ident_Rest deriving (Show, Eq)
-- newtype Ident = Ident String deriving (Show, Eq)

type Name = String
newtype Var = Var Name deriving (Show, Eq)
-- data Number = Digit Digit | Number Digit Number deriving (Show, Eq)
data ExpressionNested = ECall Call | ENum Integer | EVar Var | Exp Expression deriving (Show, Eq)
data Expression = Pos ExpressionNested | Neg ExpressionNested | Term ExpressionNested Operator ExpressionNested deriving (Show, Eq)
newtype Return = Return Var deriving (Show, Eq)
data Boolean = BExp Expression Relator Expression deriving (Show, Eq)
data Assign = Ass Var Expression deriving (Show, Eq)
data If = If Boolean Statements | Elif Boolean Statements Statements deriving (Show, Eq)
data While = While Boolean Statements deriving (Show, Eq)
data Statement = WSt While | ISt If | ASt Assign | RSt ReadSt | PSt Print deriving (Show, Eq)
data Statements = Eps | St Statement Statements deriving (Show, Eq)
data ProcedureBody = Body Statements Return deriving (Show, Eq)
data Arguments = Arg Var | Args Var Arguments deriving (Show, Eq)
data Main = Main Arguments ProcedureBody deriving (Show, Eq)

{-| --------------------------
       auxiliary functions
-}  --------------------------

isSmallLetter :: Char -> Bool
isSmallLetter c = elem c "abcdefghijklmnopqrstuvwxyz"

isOperator:: Char -> Bool
isOperator c = elem c "+-*/"

--  parse (>= 1) ASCII digits, return parsed characters
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

-- consuming whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- parse single character, return parsed character
symbol :: Char -> Parser Char
symbol c = lexeme $ char c

-- parse "(", parse ")", return enclosed characters
betweenParens :: Parser a -> Parser a
betweenParens p = between (symbol '(') (symbol ')') p

-- parse "{", parse "}", return enclosed characters
betweenParensCurly :: Parser a -> Parser a
betweenParensCurly p = between (symbol '{') (symbol '}') p

catch:: Either ParseError a -> a
catch (Left err) = error (show err)
catch (Right expr) = expr

{-| --------------------------
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

{-| --------------------------------------
         well-formed-formula parsers
-}  --------------------------------------

-- numerical expression
numE :: Parser ExpressionNested
numE = do
  x <- lexeme num
  return $ ENum x

-- variable expression
variable :: Parser Var
variable = do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isSmallLetter a)
    nonFirstChar = satisfy (\a -> isDigit a || isSmallLetter a || a == '_')

-- nested variable expression
varE :: Parser ExpressionNested
varE = do
  x <- lexeme variable
  return $ EVar x

-- postives expression
expPos :: Parser Expression
expPos = do
      x <- lexeme expNestParse
      return $ Pos x

-- negative expression
expNeg :: Parser Expression
expNeg = do
      fc <- char '-'
      x <- lexeme expNestParse
      return $ Neg x

-- combined expression
expTerm:: Parser Expression
expTerm = do
      x <- lexeme expNestParse
      o <- lexeme operatorParse
      y <- lexeme expNestParse
      return $ Term x o y

-- braced expression
bracesE :: Parser ExpressionNested
bracesE = do
      void $ lexeme $ char '('
      e <- expParse
      void $ lexeme $ char ')'
      return $ Exp e

-- boolean expression
booleanParse :: Parser Boolean
booleanParse = do
            x <- lexeme expParse
            r <- lexeme relatorParse
            y <- lexeme expParse
            return $ BExp x r y

-- combination parsers (well-formed-formula)
expNestParse :: Parser ExpressionNested
expNestParse = lexeme bracesE <|> (try $ lexeme callE) <|> lexeme varE <|> lexeme numE

relatorParse :: Parser Relator
relatorParse = try geqP <|> try leqP <|> eqP <|> neqP <|> leP <|> geP

expParse :: Parser Expression
expParse = choice [try $ lexeme expTerm, lexeme expNeg, lexeme expPos]

expParseWithEOF :: Parser Expression
expParseWithEOF = do
                whitespace
                x <- lexeme expParse
                eof
                return x

{-| --------------------------
           statements
-}  --------------------------

-- variable assignment
assignParse :: Parser Assign
assignParse = do
            x <- lexeme $ variable
            y <- lexeme $ char '='
            z <- lexeme $ expParse
            semi <- lexeme $ char ';'
            return $ Ass x z

-- if statement
ifParse' :: Parser If
ifParse' = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ If y z

-- ifelse statement
elifParse :: Parser If
elifParse = do
      x <- lexeme $ string "if"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      el <- lexeme $ string "else"
      elS <- lexeme $ betweenParensCurly statementsParse
      return $ Elif y z elS

-- if-or-ifelse statement
ifParse :: Parser If
ifParse = try elifParse <|> ifParse'

-- while statement
whileParse :: Parser While
whileParse = do
      x <- lexeme $ string "while"
      y <- lexeme $ betweenParens booleanParse
      z <- lexeme $ betweenParensCurly statementsParse
      return $ While y z

-- empty statement
emptyParse :: Parser Statements
emptyParse = return Eps

-- statement
statementParse :: Parser Statement
statementParse = (try $ fmap WSt whileParse) <|> (try $ fmap ISt ifParse) <|> (try $ fmap RSt readParse) <|> (try $ fmap PSt printParse) <|> fmap ASt assignParse

-- statements
statementsParseRec :: Parser Statements
statementsParseRec = do
            x <- lexeme statementParse
            y <- lexeme statementsParse
            return $ St x y

statementsParse :: Parser Statements
statementsParse = try statementsParseRec <|> emptyParse

{-| --------------------------
            program
-}  --------------------------

-- return statement
returnParse :: Parser Return
returnParse = do
      x <- lexeme $ string "return"
      v <- lexeme $ variable
      semi <- lexeme $ char ';'
      return $ Return v

-- procedure statement
procBodyParse :: Parser ProcedureBody
procBodyParse = do
      stat <- lexeme $ statementsParse
      ret <- lexeme $ returnParse
      return $ Body stat ret

-- procedure arguments
argVarParseRec :: Parser Arguments
argVarParseRec = do
            x <- lexeme $ variable
            col <- lexeme $ char ','
            xs <- lexeme $ argVarParse
            return $ Args x xs

argVarParse :: Parser Arguments
argVarParse = try argVarParseRec <|> (fmap Arg variable)

-- program
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

{-| ------------------------
          Extension 3.1: Procedure Calls
-}  ------------------------

data Program = Prog Main Procedures deriving (Show, Eq)
data Procedures = Nil | Procs Procedure Procedures deriving (Show, Eq)
data Procedure = Proc Var Arguments ProcedureBody deriving (Show, Eq)
data Call = Call Var ArgList deriving (Show, Eq)
data ArgList = ArgI Expression | ArgsI Expression ArgList deriving (Show, Eq)

callE :: Parser ExpressionNested
callE = do
      ident <- lexeme variable
      args <- lexeme $ betweenParens argListParse
      return $ ECall (Call ident args)

argListParseRec :: Parser ArgList
argListParseRec = do
            x <- lexeme expParse
            col <- lexeme $ char ','
            xs <- lexeme $ argListParse
            return $ ArgsI x xs

argListParse :: Parser ArgList
argListParse = try argListParseRec <|> (fmap ArgI expParse)

procParse :: Parser Procedure
procParse = do
  key <- lexeme $ string "procedure"
  ident <- lexeme variable
  argVars <- lexeme $ betweenParens argVarParse
  body <- lexeme $ betweenParensCurly procBodyParse
  return $ Proc ident argVars body

procsParseRec :: Parser Procedures
procsParseRec = do
            x <- lexeme procParse
            y <- lexeme procsParse
            return $ Procs x y

procsParse :: Parser Procedures
procsParse = try procsParseRec <|> (return Nil)

programParse :: Parser Program
programParse = do
        main <- lexeme mainParse
        procs <- lexeme procsParse
        return $ Prog main procs

programParseEOF :: Parser Program
programParseEOF = do
        whitespace
        prog <- lexeme $ programParse
        eof
        return prog


--procedure main (x) {z = proc(x,2); return x;} procedure proc (y,z) {x = y*z; z = read_int(); print_int(4*x); return x;}


{-| ------------------------
          Extension 3.1: IO
-}  ------------------------

newtype Print = Print Expression deriving (Eq, Show)
newtype ReadSt = Read Var deriving (Eq, Show)

printParse :: Parser Print
printParse = do
      string <- lexeme $ string "print_int"
      expr <- lexeme $ betweenParens expParse
      sem <- lexeme $ char ';'
      return $ Print expr

readParse :: Parser ReadSt
readParse = do
      var <- lexeme $ variable
      eq <- lexeme $ char '='
      string <- lexeme $ string "read_int();"
      return $ Read var

-- "procedure main (x) {z = x + 2; print_int(z*2); return z; }"
