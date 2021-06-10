{-|
Module      : Prettyprinter
Description : Make our abstract syntax tree an instance of Pretty
Copyright   : (c) Markus Rinke, 2021
                  Martin Gius, 2021
License     : GPL-3
Maintainer  : martin.gius@tuwien.ac.at
Stability   : experimental

-}
module Formatting where

import Prettyprinter
import ParseMINI

instance Pretty Var where
  pretty (Var name) = pretty name

instance Pretty ExpressionNested where
  pretty (ECall (Call var argList)) = pretty var <> pretty '(' <> pretty argList <> pretty ')'
  pretty (ENum int) = pretty int
  pretty (EVar var) = pretty var
  pretty (Exp expr) = pretty '(' <> pretty expr <> pretty ')'

instance Pretty Expression where
  pretty (Pos expNest) = pretty expNest
  pretty (Neg expNest) = pretty '-' <> pretty expNest
  pretty (Term exp1 op exp2) = pretty exp1 <> pretty op <> pretty exp2

instance Pretty Operator where
  pretty Plus = pretty '+'
  pretty Minus = pretty '-'
  pretty Times = pretty '*'
  pretty Divide = pretty '/'

instance Pretty Relator where
  pretty EQQ = pretty "=="
  pretty NEQ = pretty "!="
  pretty LEQ = pretty "<="
  pretty GEQ = pretty ">="
  pretty LE = pretty "<"
  pretty GE = pretty ">"

instance Pretty ArgList where
  pretty (ArgI expr) = pretty expr
  pretty (ArgsI expr args) = pretty expr <> pretty ',' <> pretty args

instance Pretty Boolean where
  pretty (BExp exp1 rel exp2) = pretty exp1 <> pretty rel <> pretty exp2

instance Pretty Assign where
  pretty (Ass var expr) = pretty var <> pretty '=' <> pretty expr <> pretty ';'

instance Pretty If where
  pretty (If bool stats) = pretty "if(" <> pretty bool <> pretty "){" <> line <> (indent 2 $ pretty stats) <> line <> pretty '}'
  pretty (Elif bool stats1 stats2) = pretty "if(" <> pretty bool <> pretty "){" <> line <> (indent 2 $ pretty stats1) <> line <> pretty '}' <> pretty "else{" <> (indent 2 $ pretty stats2) <> line <> pretty '}'

instance Pretty While where
  pretty (While bool stats) = pretty "while(" <> pretty bool <> pretty "){" <> line <> (indent 2 $ pretty stats) <> line <> pretty '}'

instance Pretty ReadSt where
  pretty (Read var) = pretty var <> pretty "=read_int();"

instance Pretty Print where
  pretty (Print expr) = pretty "print_int(" <> pretty expr <> pretty ");"

instance Pretty Statement where
  pretty (WSt wh) = pretty wh
  pretty (ISt i) = pretty i
  pretty (ASt a) = pretty a
  pretty (RSt r) = pretty r
  pretty (PSt p) = pretty p

instance Pretty Statements where
  pretty Eps = emptyDoc
  pretty (St stat stats) = pretty stat <> line <> pretty stats

instance Pretty Return where
  pretty (Return var) = pretty "return " <> pretty var <> pretty ';'

instance Pretty ProcedureBody where
  pretty (Body stats ret) = pretty stats <> line <> pretty ret

instance Pretty Arguments where
  pretty (Arg v) = pretty v
  pretty (Args v args) = pretty v <> pretty ',' <> pretty args

instance Pretty Main where
  pretty (Main args body) = pretty "procedure main(" <> pretty args <> pretty "){" <> line <> (indent 2 $ pretty body) <> line <> pretty '}'

instance Pretty Procedure where
  pretty (Proc var args body) = pretty "procedure " <> pretty var <> pretty '(' <> pretty args <> pretty "){" <> line <> (indent 2 $ pretty body) <> line <> pretty '}'

instance Pretty Procedures where
  pretty Nil = emptyDoc
  pretty (Procs pro procs) = pretty pro <> line <> pretty procs

instance Pretty Program where
  pretty (Prog main procs) = pretty main <> pretty procs
