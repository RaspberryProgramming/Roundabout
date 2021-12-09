{-
 -  HOPL/CHECKED/Interp.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.ROUNDABOUT.Interp
  ( checkAndInterp,
    checkAndInterpWith,
    interpWith,
  )
where

import Data.Either (fromRight)
import HOPL.ROUNDABOUT.Checker
import HOPL.ROUNDABOUT.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.ROUNDABOUT.Environment (Env (..))
import HOPL.ROUNDABOUT.Lang.Parser (ParseError, parseToplevel)
import HOPL.ROUNDABOUT.Lang.Syntax (Exp (..), Pgm (..), BinaryOp (..))
import HOPL.ROUNDABOUT.TypeEnv (TEnv (..), TypeEnvironment)
import HOPL.Types (Source)
import Prelude hiding (exp)
import Text.Parsec.Error (Message(Expect))

{- top-level interpreter routines -}

checkAndInterp :: Source -> Either ParseError ExpVal
checkAndInterp = checkAndInterpWith emptyTenv emptyEnv

checkAndInterpWith :: TypeEnvironment -> Environment -> Source -> Either ParseError ExpVal
checkAndInterpWith τ ρ src = flip valueOfProgram ρ <$> checkWith τ src

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv

interpWith' :: Environment -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Environment -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> ExpVal
valueOfProgram (Pgm exp) = valueOf exp

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> ExpVal
-- Variable reference
valueOf (VarExp var) ρ = applyEnv ρ var
-- Integer literal
valueOf (ConstExp n) _ = NumVal n
-- Arithmetic/numeric predicates
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ
-- Arithmetic operators
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (AddExp exp₁ exp₂) ρ = NumVal (n₁ + n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (MultExp exp₁ exp₂) ρ = NumVal (n₁ * n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (DivExp exp₁ exp₂) ρ = NumVal (abs (n₁ `div` n₂))
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (DiffAssExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
    -- TODO: Implement DiffAss and AddAss
    -- ρ' = applyEnv n₁ (n₁ - n₂) ρ 
valueOf (AddAssExp exp₁ exp₂) ρ = NumVal (n₁ + n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
    {-v = valueOf (n₁ + n₂) ρ
    ρ' = extendEnv n₁ ρ-}
-- Variable declarations
valueOf (LetExp var rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv var v ρ
    v = valueOf rhs ρ
valueOf (LetrecExp _ pname param _ pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
-- Function definition
valueOf (ProcExp param _ body) ρ = ProcVal (ClosedProcedure param body ρ)
-- Function call
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToProc (valueOf rator ρ)
valueOf (AssignExp id exp) ρ = v
  where
    ρ' = extendEnv id v ρ
    v = valueOf exp ρ
valueOf (SequenceExp [] exp') ρ = valueOf exp' ρ
valueOf (SequenceExp (exp : exps) exp') ρ = ret
  where
    v = valueOf exp ρ
    ret = valueOf (SequenceExp exps exp') ρ


valueOf (BinaryExp op exp₁ exp₂) ρ = valueOfBinaryOp op exp₁ exp₂ ρ


{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined

valueOfBinaryOp :: BinaryOp -> Exp -> Exp -> Environment -> ExpVal
valueOfBinaryOp op exp₁ exp₂ ρ = case op of
  Equal -> BoolVal (v₁ == v₂)
  NotEqual -> BoolVal (v₁ /= v₂)
  Less -> BoolVal (n₁ < n₂)
  LessEqual -> BoolVal (n₁ <= n₂)
  Greater -> BoolVal (n₁ < n₂)
  GreaterEqual -> BoolVal (n₁ <= n₂)
  where
    q₁ = expvalToBool v₁
    q₂ = expvalToBool v₂
    n₁ = expvalToNum v₁
    n₂ = expvalToNum v₂
    v₁ = valueOf exp₁ ρ
    v₂ = valueOf exp₂ ρ