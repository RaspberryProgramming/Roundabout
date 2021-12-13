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
  (
    interpWith,
    interp
  )
where

import Data.Either (fromRight)
import HOPL.ROUNDABOUT.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.ROUNDABOUT.Environment (Env (..))
import HOPL.ROUNDABOUT.Lang.Parser (ParseError, parseToplevel)
import HOPL.ROUNDABOUT.Lang.Syntax (Exp (..), Pgm (..), BinaryOp (..))
import HOPL.ROUNDABOUT.Store (Store, deref, emptyStore, newref, setref)
import HOPL.Types (Source)
import Prelude hiding (exp)
import Text.Parsec.Error (Message(Expect))

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
data Answer = Answer {getVal :: ExpVal, getStore :: Store}

{- top-level interpreter routines -}

interp :: Source -> Either ParseError (IO ExpVal)
interp = interpWith emptyEnv emptyStore

interpWith' :: Environment -> Store -> Source -> IO ExpVal
interpWith' ρ σ = fromRight undefined . interpWith ρ σ

interpWith :: Environment -> Store -> Source -> Either ParseError (IO ExpVal)
interpWith ρ σ src = flip (`valueOfProgram` ρ) σ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> Store -> IO ExpVal
valueOfProgram (Pgm exp) ρ σ = getVal <$> valueOf exp ρ σ

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> Store -> IO Answer
valueOf (VarExp x) ρ σ = return (Answer (deref addr σ) σ)
  where
    addr = applyEnv ρ x
valueOf (ConstExp n) _ σ = return $ Answer (NumVal n) σ

valueOf (IsZeroExp exp₁) ρ σ = do
  Answer (NumVal n) σ₁ <- valueOf exp₁ ρ σ
  return $ Answer (BoolVal (n == 0)) σ₁
    
valueOf (DiffExp exp₁ exp₂) ρ σ = do
  Answer (NumVal n₁) σ₁ <- valueOf exp₁ ρ σ
  Answer (NumVal n₂) σ₂ <- valueOf exp₂ ρ σ₁
  return $ Answer (NumVal (n₁ - n₂)) σ₂
valueOf (AddExp exp₁ exp₂) ρ σ = do
  Answer (NumVal n₁) σ₁ <- valueOf exp₁ ρ σ
  Answer (NumVal n₂) σ₂ <- valueOf exp₂ ρ σ₁
  return $ Answer (NumVal (n₁ + n₂)) σ₂
valueOf (MultExp exp₁ exp₂) ρ σ = do
  Answer (NumVal n₁) σ₁ <- valueOf exp₁ ρ σ
  Answer (NumVal n₂) σ₂ <- valueOf exp₂ ρ σ₁
  return $ Answer (NumVal (n₁ * n₂)) σ₂
valueOf (DivExp exp₁ exp₂) ρ σ = do
  Answer (NumVal n₁) σ₁ <- valueOf exp₁ ρ σ
  Answer (NumVal n₂) σ₂ <- valueOf exp₂ ρ σ₁
  return $ Answer (NumVal (abs (n₁ `div` n₂))) σ₂
valueOf (DiffAssExp x exp) ρ σ = do
  let addr = applyEnv ρ x
  let NumVal v = deref addr σ
  Answer (NumVal n₁) σ₁ <- valueOf exp ρ σ
  let res = NumVal (v-n₁)
  let σ₂ = setref (applyEnv ρ x) res σ₁
  return $ Answer (res) σ₂
valueOf (AddAssExp x exp) ρ σ = do
  let addr = applyEnv ρ x
  let NumVal v = deref addr σ
  Answer (NumVal n₁) σ₁ <- valueOf exp ρ σ
  let res = NumVal (v+n₁)
  let σ₂ = setref (applyEnv ρ x) res σ₁
  return $ Answer (res) σ₂
{-valueOf EmptyExp ρ σ = return (Answer ( ListVal [] ) σ)
valueOf (ListExp exps) ρ σ = do
  valof n = ret
    where
      ret <- valueOf n ρ σ
  let vsa = map valof exps 
  let vs = map getVal vsa
  return (Answer (ListVal vs) σ)
-}
{-
 -- List constructors
valueOf EmptyExp ρ σ = Answer ( ListVal [] ) σ
valueOf (ListExp exps) ρ σ = Answer (ListVal vs) σ
  where
  valof n = valueOf n ρ σ
  vsa = map valof exps 
  vs = map getVal vsa
-- Variable declarations
-}
valueOf (LetExp x rhs body) ρ σ = do
  Answer v σ₁ <- valueOf rhs ρ σ -- <- extracts from IO layer
  let (addr, σ₂) = newref v σ₁
  let ρ' = extendEnv x addr ρ
  valueOf body ρ' σ₂ -- Already wrapped in IO layer, return wrapps items in IO layer
    
{-
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
    ans = valueOf exp' ρ σ
    v = getVal ans
    σ₁ = getStore ans
    
valueOf (SequenceExp (exp : exps) exp') ρ σ = Answer retVal σ₁
  where
    v = valueOf exp ρ σ
    σ₁ = getStore v
    ret = valueOf (SequenceExp exps exp') ρ σ₁
    retVal = getVal ret
    σ₂ = getStore ret

valueOf (LoopExp exp₁ exp₂) ρ σ = Answer retVal σ₂
  where
    expAns = valueOf exp₂ ρ σ
    σ₁ = getStore expAns

    cont = getVal (valueOf exp₁ ρ σ₁)

    ret = case cont of
      BoolVal True -> valueOf (LoopExp exp₁ exp₂) ρ σ₁
      BoolVal False -> expAns
    
    σ₂ = getStore ret
    retVal = getVal ret

valueOf (BinaryExp op exp₁ exp₂) ρ σ = Answer(valueOfBinaryOp op exp₁ exp₂ ρ σ) σ
valueOf (StringExp s) _ σ = Answer (StrVal s) σ
valueOf (LookupExp exp₁ exp₂) ρ σ = Answer v σ
  where
    Answer (NumVal loc) σ₁ = valueOf exp₂ ρ σ
    Answer (ListVal list) σ₂ = valueOf exp₁ ρ σ
    -- loc = NumVal (getVal (valueOf exp₂ ρ σ))
    v = list !! fromInteger loc
valueOf (PrintExp exp₁) ρ σ = Answer v σ
  where
    v = getVal ( valueOf exp₁ ρ σ)
    x = show v
    -- show v-}

{- Auxiliary function for applying procedure values -}
{-applyProcedure :: Procedure -> DenVal -> Store -> Answer
applyProcedure (ClosedProcedure x body ρ) arg σ = valueOf body (extendEnv x arg ρ) σ
applyProcedure _ _ _ = undefined-}

{- valueOfBinaryOp :: BinaryOp -> Exp -> Exp -> Environment -> Store -> ExpVal
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
    v₂ = getVal (valueOf exp₂ ρ σ) -}