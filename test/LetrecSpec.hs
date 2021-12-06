{-
 -  ProcSpec.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the LETREC interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module LetrecSpec (spec) where

import Data.Either (fromRight)
import HOPL.LETREC.DataStructures (ExpVal (..))
import HOPL.LETREC.Environment (Env (emptyEnv, extendEnv'))
import HOPL.LETREC.Interp (interpWith)
import Test.Tasty.Hspec

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [NumVal 1, NumVal 5, NumVal 10]
    emptyEnv

spec =
  describe "LETREC tests" $ do

    describe "Value tests" $ do
      specify "positive-const" $
        interp "11" `shouldBe` NumVal 11
      specify "negative-const" $
        interp "-33" `shouldBe` NumVal (-33)
      specify "simple-arith-1" $
        interp "-(44,33)" `shouldBe` NumVal 11
      specify "nested-arith-left" $
        interp "-(-(44,33),22)" `shouldBe` NumVal (-11)
      specify "nested-arith-right" $
        interp "-(55, -(22,11))" `shouldBe` NumVal 44
      specify "test-var-1" $
        interp "x" `shouldBe` NumVal 10
      specify "test-var-2" $
        interp "-(x,1)" `shouldBe` NumVal 9
      specify "test-var-3" $
        interp "-(1,x)" `shouldBe` NumVal (-9)
      specify "if-true" $
        interp "if zero?(0) then 3 else 4" `shouldBe` NumVal 3
      specify "if-false" $
        interp "if zero?(1) then 3 else 4" `shouldBe` NumVal 4
      specify "if-eval-test-true" $
        interp "if zero?(-(11,11)) then 3 else 4" `shouldBe` NumVal 3
      specify "if-eval-test-false" $
        interp "if zero?(-(11, 12)) then 3 else 4" `shouldBe` NumVal 4
      specify "if-eval-test-true-2" $
        interp "if zero?(-(11, 11)) then 3 else foo" `shouldBe` NumVal 3
      specify "if-eval-test-false-2" $
        interp "if zero?(-(11,12)) then foo else 4" `shouldBe` NumVal 4
      specify "simple-let-1" $
        interp "let x = 3 in x" `shouldBe` NumVal 3
      specify "eval-let-body" $
        interp "let x = 3 in -(x,1)" `shouldBe` NumVal 2
      specify "eval-let-rhs" $
        interp "let x = -(4,1) in -(x,1)" `shouldBe` NumVal 2
      specify "simple-nested-let" $
        interp "let x = 3 in let y = 4 in -(x,y)" `shouldBe` NumVal (-1)
      specify "check-shadowing-in-body" $
        interp "let x = 3 in let x = 4 in x" `shouldBe` NumVal 4
      specify "check-shadowing-in-rhs" $
        interp "let x = 3 in let x = -(x,1) in x" `shouldBe` NumVal 2
      specify "apply-proc-in-rator-pos" $
        interp "(proc(x) -(x,1)  30)" `shouldBe` NumVal 29
      specify "apply-simple-proc" $
        interp "let f = proc (x) -(x,1) in (f 30)" `shouldBe` NumVal 29
      specify "let-to-proc-1" $
        interp "(proc(f)(f 30)  proc(x)-(x,1))" `shouldBe` NumVal 29
      specify "nested-procs" $
        interp "((proc (x) proc (y) -(x,y)  5) 6)" `shouldBe` NumVal (-1)
      specify "nested-procs2" $
        interp "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
          `shouldBe` NumVal (-1)
      specify "y-combinator-1" $
        interp "let fix =  proc (f) \
               \             let d = proc (x) proc (z) ((f (x x)) z) \
               \             in proc (n) ((f (d d)) n) \
               \in let t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4) \
               \   in let times4 = (fix t4m) \
               \      in (times4 3)" `shouldBe` NumVal 12
      specify "simple-letrec-1" $
        interp "letrec f(x) = -(x,1) in (f 33)" `shouldBe` NumVal 32
      specify "simple-letrec-2" $
        interp "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)" `shouldBe` NumVal 8
      specify "simple-letrec-3" $
        interp "let m = -5 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)" `shouldBe` NumVal 20
      specify "HO-nested-letrecs" $
        interp "letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1)) \
               \in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1)) \
               \   in (odd 13)" `shouldBe` NumVal 1

    describe "Exception tests" $ do
      specify "no-bool-to-diff-1" $
        printInterp "-(zero?(0),1)" `shouldThrow` anyException
      specify "no-bool-to-diff-2" $
        printInterp "-(1,zero?(0))" `shouldThrow` anyException
      specify "no-int-to-if" $
        printInterp "if 1 then 2 else 3" `shouldThrow` anyException

    describe "Error tests" $ do
      specify "test-unbound-var-1" $
        printInterp "foo" `shouldThrow` anyErrorCall
      specify "test-unbound-var-1" $
        printInterp "-(x,foo)" `shouldThrow` anyErrorCall
  where
    interp = fromRight undefined . interpWith testEnv
    printInterp = print . interpWith testEnv
