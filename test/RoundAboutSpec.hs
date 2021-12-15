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
import HOPL.ROUNDABOUT.DataStructures (ExpVal (..))
import HOPL.ROUNDABOUT.Environment (Env (emptyEnv, extendEnv'))
import HOPL.ROUNDABOUT.Interp (interp, interpWith)
import Test.Tasty.Hspec

testStore = [NumVal 1, NumVal 5, NumVal 10]

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [0, 1, 2]
    emptyEnv

spec =
  describe "ROUNDABOUT tests" $ do
    describe "Value tests" $ do
      -- simple arithmetic
      specify "positive-const" $
        interp "11" `shouldBe` NumVal 11
      specify "loop-test" $
        interp "let x = 0 in let y = 1 in loop <(x,y) in {assign x = +(x,1) return x}" `shouldBe` NumVal 1
      specify "list-lookup-test" $
        interp "let x = [5,10,20] in x[1]" `shouldBe` NumVal 10
      specify "add-test" $
        interp "let x = 1 in +(x,2)" `shouldBe` NumVal 3
      specify "diff-test" $
        interp "let x = 1 in -(2,x)" `shouldBe` NumVal 1
      specify "mult-test" $
        interp "let x = 1 in +(x,2)" `shouldBe` NumVal 2
      specify "div-test" $
        interp "let x = 4 in +(x,2)" `shouldBe` NumVal 2
      specify "add-assign-ret-test" $
        interp "let x = 1 in +=(x,2)" `shouldBe` NumVal 3
      specify "diff-assign-ret-test" $
        interp "let x = 1 in -=(2,x)" `shouldBe` NumVal 1
  where
    interp = fromRight undefined . interpWith testEnv testStore
    printInterp = print . interpWith testEnv testStore

