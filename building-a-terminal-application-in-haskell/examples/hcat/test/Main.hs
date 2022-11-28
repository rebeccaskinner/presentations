module Main where

import Test.Hspec
import qualified HCatSpec

main = hspec $ do
  testStub

testStub :: Spec
testStub =
  describe "placeholder" $ do
    it "is pending" $ do
      pending
