module Main
  ( main
  ) where

import Test.CHT (chtTests)

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  chtUTests <- chtTests
  let allTests = testGroup "HW4 tests" [chtUTests]
  defaultMain allTests
