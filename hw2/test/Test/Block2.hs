module Test.Block2
  ( evalTests
  ) where

import Block2 (ArithmeticError (..), eval, Expr (..))

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

evalHspecTest :: Spec
evalHspecTest = do
  describe "dividing by zero fails with ArithmeticError" $ do
    it "1 / 0" $
      eval (Div (Const 1) (Const 0)) `shouldBe` Left DivByZero
    it "1 / (1 - 1)" $
      eval (Div (Const 1) (Sub (Const 1) (Const 1))) `shouldBe` Left DivByZero
    it "1 ^ (1 / (3 - 3))" $
      eval (Pow (Const 1) (Div (Const 1) (Sub (Const 3) (Const 3)))) `shouldBe` Left DivByZero
  describe "negative pow fails with ArithmeticError" $ do
    it "1 ^ (-1)" $
      eval (Pow (Const 1) (Const (negate 1))) `shouldBe` Left NegExp
    it "5 ^ (3 - 4)" $
      eval (Pow (Const 5) (Sub (Const 3) (Const 4))) `shouldBe` Left NegExp
    it "1 ^ (2 ^ (1 * (-2)))" $
      eval (Pow (Const 1) (Pow (Const 2) (Mul (Const 1) (Const (negate 2))))) `shouldBe` Left NegExp
  describe "result is correct" $ do
    it "12 + 13" $
      eval (Add (Const 12) (Const 13)) `shouldBe` Right 25
    it "119 - 110 * 2" $
      eval (Sub (Const 119) (Mul (Const 110) (Const 2))) `shouldBe` Right (negate 101)
    it "128 / 64" $
      eval (Div (Const 128) (Const 64)) `shouldBe` Right 2
    it "(7 - 1) ^ (3 - 3)" $
      eval (Pow (Sub (Const 7) (Const 1)) (Sub (Const 3) (Const 3))) `shouldBe` Right 1
    it "2 ^ 10" $
      eval (Pow (Const 2) (Const 10)) `shouldBe` Right 1024

evalTests :: IO TestTree
evalTests = testSpec "eval tests" evalHspecTest
