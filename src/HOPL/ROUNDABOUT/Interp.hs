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
import HOPL.ROUNDABOUT.Store (Store, deref, emptyStore, newref, setref)
import HOPL.ROUNDABOUT.TypeEnv (TEnv (..), TypeEnvironment)
import HOPL.Types (Source)
import Prelude hiding (exp)
import Text.Parsec.Error (Message(Expect))

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
data Answer = Answer {getVal :: ExpVal, getStore :: Store}

{- top-level interpreter routines -}

checkAndInterp :: Source -> Either ParseError ExpVal
checkAndInterp = checkAndInterpWith emptyTenv emptyEnv emptyStore

checkAndInterpWith :: TypeEnvironment -> Environment -> Store -> Source -> Either ParseError ExpVal
checkAndInterpWith τ ρ σ src = flip (`valueOfProgram` ρ) σ <$> checkWith τ src

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv emptyStore

interpWith' :: Environment -> Store -> Source -> ExpVal
interpWith' ρ σ = fromRight undefined . interpWith ρ σ

interpWith :: Environment -> Store -> Source -> Either ParseError ExpVal
interpWith ρ σ src = flip (`valueOfProgram` ρ) σ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> Store -> ExpVal
valueOfProgram (Pgm exp) ρ σ = getVal (valueOf exp ρ σ)

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> Store -> Answer
valueOf (VarExp x) ρ σ = Answer (deref addr σ) σ
  where
    addr = applyEnv ρ x
valueOf (ConstExp n) _ σ = Answer (NumVal n) σ
valueOf (IsZeroExp exp₁) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf exp₁ ρ σ
valueOf (DiffExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (AddExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ + n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (MultExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ * n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (DivExp exp₁ exp₂) ρ σ = Answer (NumVal (abs (n₁ `div` n₂))) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (DiffAssExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
    -- TODO: Implement DiffAss and AddAss
    -- ρ' = applyEnv n₁ (n₁ - n₂) ρ 
valueOf (AddAssExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ + n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
    {-v = valueOf (n₁ + n₂) ρ
    ρ' = extendEnv n₁ ρ-}
-- Variable declarations
valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₂
  where
    ρ' = extendEnv x addr ρ
    (addr, σ₂) = newref v σ₁
    Answer v σ₁ = valueOf rhs ρ σ
valueOf (IfExp exp₁ exp₂ exp₃) ρ σ = valueOf exp' ρ σ₁
  where
    Answer q σ₁ = valueOf exp₁ ρ σ
    exp' = case q of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
valueOf (ProcExp x body) ρ σ = Answer (ProcVal (ClosedProcedure x body ρ)) σ
valueOf (CallExp rator rand) ρ σ = applyProcedure (expvalToProc f) addr σ₃
  where
    Answer f σ₁ = valueOf rator ρ σ
    Answer v σ₂ = valueOf rand ρ σ₁
    (addr, σ₃) = newref v σ₂
valueOf (AssignExp var rhs) ρ σ = Answer rval σ₂
  where
    Answer rval σ₁ = valueOf rhs ρ σ
    σ₂ = setref (applyEnv ρ var) rval σ₁
valueOf (SequenceExp [] exp') ρ σ = Answer v σ
  where
    a = valueOf exp' ρ σ
    v = getVal a
valueOf (SequenceExp (exp : exps) exp') ρ σ = Answer retVal σ
  where
    v = valueOf exp ρ σ
    ret = valueOf (SequenceExp exps exp') ρ σ
    retVal = getVal ret



valueOf (BinaryExp op exp₁ exp₂) ρ σ = Answer(valueOfBinaryOp op exp₁ exp₂ ρ σ) σ


{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> Store -> Answer
applyProcedure (ClosedProcedure x body ρ) arg σ = valueOf body (extendEnv x arg ρ) σ
applyProcedure _ _ _ = undefined

valueOfBinaryOp :: BinaryOp -> Exp -> Exp -> Environment -> Store -> ExpVal
valueOfBinaryOp op exp₁ exp₂ ρ σ = case op of
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
    v₁ = getVal (valueOf exp₁ ρ σ)
    v₂ = getVal (valueOf exp₂ ρ σ)