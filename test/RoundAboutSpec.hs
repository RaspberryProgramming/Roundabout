{-
 -  RoundAboutSpec.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the CHECKED interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module RoundAboutSpec (spec) where

import Data.Either (fromRight)
import HOPL.ROUNDABOUT.Checker (checkWith, parseToplevel, typeOfProgram)
import HOPL.ROUNDABOUT.DataStructures (ExpVal (..))
import HOPL.ROUNDABOUT.Environment (Env (emptyEnv, extendEnv'))
import HOPL.ROUNDABOUT.Interp (checkAndInterpWith, interpWith)
import HOPL.ROUNDABOUT.Type (Type (..))
import HOPL.ROUNDABOUT.TypeEnv
    ( Type(IntType), TEnv(emptyTenv, extendTenv') )
import Test.Tasty.Hspec

testStore = [NumVal 1, NumVal 5, NumVal 10]

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [0, 1, 2]
    emptyEnv

testTenv =
  extendTenv'
    ["i", "v", "x"]
    [IntType, IntType, IntType]
    emptyTenv

spec =
  describe "CHECKED tests" $ do
    describe "Value tests" $ do
      -- simple arithmetic
      specify "positive-const" $
        interp "11" `shouldBe` NumVal 11
      specify "" $
        interp "let x = 0 in let y = 1 in loop <(x,y) in {assign x = +(x,1) return x}" `shouldBe` NumVal 1
  where
    interp = fromRight undefined . interpWith testEnv testStore
    printInterp = print . interpWith testEnv
    check = fromRight undefined . (flip typeOfProgram testTenv <$>) . parseToplevel
    printCheck = print . (flip typeOfProgram testTenv <$>) . parseToplevel
