module Test.Block3
  ( parsersTests
  ) where

import Block3 ( element
              , eof
              , ok
              , Parser (..)
              , pspEofParser
              , satisfy
              , stream
              )

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

parsersHspecTest :: Spec
parsersHspecTest = do
  describe "ok parser" $ do
    it "\"asd\"" $
      runParser ok "asd" `shouldBe` Just ((), "asd")
    it "\"123\"" $
      runParser ok "123" `shouldBe` Just ((), "123")
  describe "eof parser" $ do
    it "\"119\"" $
      runParser eof "119" `shouldBe` Nothing
    it "" $
      runParser eof "" `shouldBe` Just ((), [])
  describe "satisfy parser" $ do
    it "satisfy (== \'1\') \"123\"" $
      runParser (satisfy (== '1')) "123" `shouldBe` Just ('1', "23")
    it "satisfy (== '2') \"119\"" $
      runParser (satisfy (== '2')) "119" `shouldBe` Nothing
  describe "element parser" $ do
    it "element \'1\' \"123\"" $
      runParser (element '1') "123" `shouldBe` Just ('1', "23")
    it "element '2' \"119\"" $
      runParser (element '2') "119" `shouldBe` Nothing
  describe "stream parser" $ do
    it "stream \"12\" \"123\"" $
      runParser (stream "12") "123" `shouldBe` Just ("12", "3")
    it "stream \"12\" \"119\"" $
      runParser (stream "12") "119" `shouldBe` Nothing
  describe "psp paraser" $ do
    it "()()(())" $
      runParser pspEofParser "()()(())" `shouldBe` Just ((), [])
    it "()()())" $
      runParser pspEofParser "()()())" `shouldBe` Nothing

parsersTests :: IO TestTree
parsersTests = testSpec "parsers tests" parsersHspecTest
