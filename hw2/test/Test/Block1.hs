module Test.Block1
  ( stringSumTests
  ) where

import Data.Maybe (isNothing)

import Block1 (stringSum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

stringSumHspecTest :: Spec
stringSumHspecTest = do
  describe "not correct input supported" $ do
    it "\"a\"" $
      stringSum "a" `shouldSatisfy` isNothing
    it "\"123 a 123\"" $
      stringSum "123 a 123" `shouldSatisfy` isNothing
    it "\"1230q 111\"" $
      stringSum "1230q 111" `shouldSatisfy` isNothing
    it "\"qwertyuiopkjhgfdsdfgh\"" $
      stringSum "qwertyuiopkjhgfdsdfgh" `shouldSatisfy` isNothing
    it "\"123 45678-4567890\"" $
      stringSum "123 45678-4567890" `shouldSatisfy` isNothing
  describe "whitespaces does not affect result" $ do
    it "\"  2        3\"" $
      stringSum "  2        3" `shouldBe` Just 5
    it "\"  2 3  \"" $
      stringSum "  2 3  " `shouldBe` Just 5
    it "\"2 3\"" $
      stringSum "2 3" `shouldBe` Just 5
  describe "result is correct" $ do
    it "\"11 11 11 11 11    11  11  \"" $
      stringSum "11 11 11 11 11    11  11  " `shouldBe` Just 77
    it "\"  22 11 1  11  \"" $
      stringSum "  22 11 1  11  " `shouldBe` Just 45
    it "\"1 2 3\"" $
      stringSum "1 2 3" `shouldBe` Just 6
    it "\"0 0 0 0 0\"" $
      stringSum "0 0 0 0 0" `shouldBe` Just 0
    it "\"\"" $
      stringSum "" `shouldBe` Just 0

stringSumTests :: IO TestTree
stringSumTests = testSpec "stringSum tests" stringSumHspecTest
