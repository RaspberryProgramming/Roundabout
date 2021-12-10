{-
 -  HOPL/CHECKED/Syntax.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.ROUNDABOUT.Lang.Syntax where

import HOPL.ROUNDABOUT.Type (Type)
import HOPL.Types (Id, Op)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

-- For each non-terminal appearing on the right-hand side of a production
-- we include a parameter type for the corresponding data constructor.
data Exp
  = -- Variable reference
    VarExp Id
  | -- Integer literal
    ConstExp Integer
  | -- Arithmetic/numeric predicates
    IsZeroExp Exp
  | -- Arithmetic operators
    DiffExp Exp Exp
  | AddExp Exp Exp
  | DivExp Exp Exp
  | MultExp Exp Exp
  | AddAssExp Exp Exp
  | DiffAssExp Exp Exp
  | -- Variable declarations
    LetExp Id Exp Exp
  | LetrecExp Type Id Id Type Exp Exp
  | -- Control expressions
    IfExp Exp Exp Exp
   -- Function definition
  |  ProcExp Id Type Exp
  |  LoopExp Exp Exp
 -- | CharExp Char
 -- | IntExp Int
 -- | StringExp Char Char Char...
 -- ! | EmptyListExp Type ListVal Type 
 -- ! | ListExp Type [Exp] Exp Exp
 -- | listLookupExp Exp Int
 -- | strLookupExp Exp Int
 -- | listAssignExp Exp Int
 -- | strAssignExp Exp Int
 -- | strConcatExp Exp Exp
  | FunctExp Id [Id] Exp
  | AssignExp Id Exp
  | SequenceExp [Exp] Exp
 -- | functCallExp Indentifier [Exp]
  | BinaryExp BinaryOp Exp Exp
    -- Function call
  |  CallExp Exp Exp
  deriving (Eq, Ord, Show)

data BinaryOp
  = Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  deriving (Eq, Ord, Show)