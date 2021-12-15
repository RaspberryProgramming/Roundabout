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

ioWrap :: Answer -> IO Answer
ioWrap ans = do return ans

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
-- valueOf EmptyExp ρ σ = return (Answer ( ListVal [] ) σ)
valueOf (ListExp []) ρ σ = do
  return (Answer (ListVal []) σ)
valueOf (ListExp (exp : exps)) ρ σ = do
  Answer (v₁) σ₁ <- valueOf (exp) ρ σ
  -- let vsa = map valof exps 
  -- let vs = map getVal vsa
  
  Answer (ListVal v) σ₂ <- valueOf (ListExp exps) ρ σ₁
  let vs =  [v₁] ++ v;
  return (Answer (ListVal vs) σ₂)

valueOf (LetExp x rhs body) ρ σ = do
  Answer v σ₁ <- valueOf rhs ρ σ -- <- extracts from IO layer
  let (addr, σ₂) = newref v σ₁
  let ρ' = extendEnv x addr ρ
  valueOf body ρ' σ₂ -- Already wrapped in IO layer, return wrapps items in IO layer
    
valueOf (IfExp exp₁ exp₂ exp₃) ρ σ = do
  
  Answer q σ₁ <- valueOf exp₁ ρ σ

  exp' <- case q of
      BoolVal True -> return exp₂
      _ -> return exp₃

  Answer v₂ σ <- valueOf exp' ρ σ₁
  return (Answer v₂ σ)

valueOf (ProcExp x body) ρ σ = do
  return (Answer (ProcVal (ClosedProcedure x body ρ)) σ)
valueOf (CallExp rator rand) ρ σ = do
  Answer f σ₁ <- valueOf rator ρ σ
  Answer v σ₂ <- valueOf rand ρ σ₁
  let (addr, σ₃) = newref v σ₂
  applyProcedure (expvalToProc f) addr σ₃

valueOf (AssignExp var rhs) ρ σ = do
  Answer rval σ₁ <- valueOf rhs ρ σ
  let σ₂ = setref (applyEnv ρ var) rval σ₁
  return (Answer rval σ₂)

valueOf (SequenceExp [] exp') ρ σ = do
  Answer v σ₁ <- valueOf exp' ρ σ
  return (Answer v σ₁)

valueOf (SequenceExp (exp : exps) exp') ρ σ = do
  Answer v σ₁ <- valueOf exp ρ σ
  Answer ret σ₂ <- valueOf (SequenceExp exps exp') ρ σ₁
  return (Answer ret σ₂)
    -- retVal = getVal ret
    -- σ₂ = getStore ret
valueOf (LoopExp exp₁ exp₂) ρ σ = do
    Answer expVal σ₁ <- valueOf exp₂ ρ σ

    Answer cont σ₂ <- valueOf exp₁ ρ σ₁

    Answer retVal σ₃ <- case cont of
      BoolVal True -> valueOf (LoopExp exp₁ exp₂) ρ σ₂
      _ -> ioWrap (Answer expVal σ₂)

    return (Answer retVal σ₃)

valueOf (BinaryExp op exp₁ exp₂) ρ σ = do 
  v <- valueOfBinaryOp op exp₁ exp₂ ρ σ
  return (Answer(v) σ)
valueOf (StringExp s) _ σ = do
  return (Answer (StrVal s) σ)
valueOf (LookupExp exp₁ exp₂) ρ σ = do
  Answer (NumVal loc) σ₁ <- valueOf exp₂ ρ σ
  Answer (val) σ₂ <- valueOf exp₁ ρ σ

  Answer v _ <- case val of
    ListVal x -> ioWrap (Answer( x !! fromInteger loc) undefined)
    StrVal x -> ioWrap (Answer( StrVal [(x !! fromInteger loc)]) undefined)
    _ -> ioWrap (Answer undefined undefined)

  return (Answer v σ)

{-valueOf (StrLookupExp x exp₂) ρ σ = do
  Answer (NumVal loc) σ₁ <- valueOf exp₂ ρ σ
  Answer (StrVal str) σ₂ <- valueOf (VarExp x) ρ σ₁
  let v = take (fromInteger loc) str -- !! fromInteger loc

  return (Answer (StrVal v) σ₁)-}
    
valueOf (PrintExp exp₁) ρ σ = do
  Answer v σ₁ <- valueOf exp₁ ρ σ
  print v
  return (Answer v σ₁)

{-~ Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> Store -> IO Answer
applyProcedure (ClosedProcedure x body ρ) arg σ = do 
  Answer v₁ σ₁ <- valueOf body (extendEnv x arg ρ) σ
  return (Answer v₁ σ₁)
applyProcedure _ _ _ = undefined

valueOfBinaryOp :: BinaryOp -> Exp -> Exp -> Environment -> Store -> IO ExpVal
valueOfBinaryOp op exp₁ exp₂ ρ σ = do
  -- e₁ <- exp₁
  Answer v₁ σ₁ <- valueOf exp₁ ρ σ
  Answer v₂ σ₂ <- valueOf exp₂ ρ σ₁
  let q₁ = expvalToBool v₁
  let q₂ = expvalToBool v₂
  let n₁ = expvalToNum v₁
  let n₂ = expvalToNum v₂
  return (case op of
    Equal -> BoolVal (v₁ == v₂)
    NotEqual -> BoolVal (v₁ /= v₂)
    Less -> BoolVal (n₁ < n₂)
    LessEqual -> BoolVal (n₁ <= n₂)
    Greater -> BoolVal (n₁ > n₂)
    GreaterEqual -> BoolVal (n₁ >= n₂))
