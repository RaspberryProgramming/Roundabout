{-
 -  HOPL/CHECKED/Checker.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the static type checker implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.ROUNDABOUT.Checker (check, checkWith, parseToplevel, typeOfProgram) where

import HOPL.ROUNDABOUT.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.ROUNDABOUT.Environment (Env (..))
import HOPL.ROUNDABOUT.Lang.Parser (ParseError, parseToplevel)
import HOPL.ROUNDABOUT.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.ROUNDABOUT.TypeEnv
import HOPL.Types (Source)

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith τ src = case result of
  Right prog -> typeOfProgram prog τ `seq` result
  _ -> result
  where
    result = parseToplevel src

reportUnequalTypes :: Type -> Type -> Exp -> Type
reportUnequalTypes t₁ t₂ exp =
  error $
    "Types didn't match: "
      ++ show t₁
      ++ " /= "
      ++ show t₂
      ++ " in "
      ++ show (show exp)

typeOfProgram :: Pgm -> TypeEnvironment -> Type
typeOfProgram (Pgm e) τ = typeOf e τ

typeOf :: Exp -> TypeEnvironment -> Type
typeOf (ConstExp _) _ = IntType
typeOf (VarExp x) τ = applyTenv τ x
typeOf (IsZeroExp exp) τ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp τ
-- Arithmetic
typeOf (DiffExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (AddExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (DivExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (MultExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (AddAssExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (DiffAssExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (BinaryExp op exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (AssignExp var exp₁ ) τ = t
  where
    τ' = extendTenv var t τ
    t = typeOf exp₁ τ
typeOf (IfExp exp₁ exp₂ exp₃) τ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | t₂ /= t₃ = reportUnequalTypes t₂ t₃ exp₂
  | otherwise = t₂
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
    t₃ = typeOf exp₃ τ
typeOf (LetExp var rhs body) τ = typeOf body τ'
  where
    τ' = extendTenv var t τ
    t = typeOf rhs τ
typeOf (LetrecExp tres pname param targ pbody body) τ
  | tres' == tres = typeOf body τ'
  | otherwise = reportUnequalTypes tres tres' pbody
  where
    τ' = extendTenv pname (ProcType targ tres) τ
    tres' = typeOf pbody (extendTenv param targ τ')
typeOf (ProcExp param body) τ = tres
  where
    tres = typeOf body τ
typeOf (CallExp rator rand) τ
  | targ == targ' = tres
  | otherwise = reportUnequalTypes targ targ' rand
  where
    ProcType targ tres = typeOf rator τ
    targ' = typeOf rand τ

typeOf (SequenceExp exps ret ) τ = t
  where
    t = typeOf ret τ

{--- Auxiliary functions ---}
