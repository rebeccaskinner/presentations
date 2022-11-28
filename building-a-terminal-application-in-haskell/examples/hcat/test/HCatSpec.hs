{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
--START:HCatInitialTests
module HCatSpec where

import Test.Hspec
import Test.QuickCheck
import qualified System.IO.Error as IOError
import qualified Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as Env
import qualified Text.Printf as Printf
import qualified HCat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test testing" test_Testing
  describe "runHCat returns an IO action" test_runHCat
--END:HCatInitialtests
  describe "test getting filename from args" test_handleArgs
  describe "test converting either to an io error" test_eitherToErr

test_Testing :: Spec
test_Testing = do
  context "when run" $ do
    it "fails" $ do
      0 `shouldBe` 1

--START:HCatInitialTests
test_runHCat :: Spec
test_runHCat = do
  context "when called" $
    it "returns an IO Unit" $
      HCat.runHCat `shouldReturn` ()
--END:HCatInitialTests

--START:test_handleArgs
test_handleArgs :: Spec
test_handleArgs = do
  context "when a single list item" $ do
    it "returns the list element" $ property $
      \(PrintableString str) ->
        Env.withArgs [str] HCat.handleArgs `shouldReturn` (Right str)
--END:test_handleArgs
--START:testHAEL
  context "when an empty list" $ do
    it "returns an error message" $
        Env.withArgs [] HCat.handleArgs
                    `shouldReturn` (Left "no filename provided")
--END:testHAEL

--START:test_groupsOf
test_groupsOf :: Spec
test_groupsOf = do
  context "when non-empty list" $ do
    it "does not lose any elements" $ property $
      \(NonEmpty lst) (Positive groupSize) ->
        let groups = HCat.groupsOf @Int groupSize  lst
        in length lst `shouldBe` (sum $ map length groups)
    it "ensures all but the last element are group size" $ property $
      \(NonEmpty lst) (Positive groupSize) ->
        let groups = init $ HCat.groupsOf @String groupSize lst
        in all (== groupSize) (map length groups) `shouldBe` True
  context "when empty list" $ do
    it "returns an empty list" $ property $
      \(Positive size) ->
        HCat.groupsOf @Int size [] `shouldBe` []
  context "when the group size is 0" $ do
    it "should raise an exception" $ property $
      \(NonEmpty lst) ->
        let groups = HCat.groupsOf @Int 0 lst
        in (Control.Exception.evaluate groups)  `shouldThrow` anyErrorCall
--END:test_groupsOf

--START:test_wordWrap
test_wordWrap :: Spec
test_wordWrap = do
  it "returns words in the right order" $ property $
    \str' (Positive width) ->
      let str = BS.pack str'
          words =  concatMap BS.words $ HCat.wordWrap width str
      in words `shouldBe` (BS.words str)
  it "does not break a word" $ property $
    \(Positive width) ->
      let width' = width + 5
          str = BS.pack $ replicate width' 'a'
          wrapped = HCat.wordWrap width str
      in do
        (length wrapped) `shouldBe` 1
        (head wrapped) `shouldBe` str
  it "breaks lines to the appropriate length" $ property $
    \str' (Positive width) ->
      let str = BS.pack str'
          lines = HCat.wordWrap width str
      in shouldSatisfy lines $ all $ \line ->
          (BS.length line) <= width ||
          (length . BS.words $ line) == 1
--END:test_wordWrap

test_eitherToErr :: Spec
test_eitherToErr = do
  context "When called with a Right value" $
    it "doesn't raise an exception" $ property $
      \val ->
          HCat.eitherToErr (Right @String @Int val) `shouldReturn` val
  context "when called with a Left value" $
    it "throws an IO exception" $ property $
      \(PrintableString err) ->
        let eitherErrVal = Left err :: Either String ()
            expected = IOError.userError (show err)
        in HCat.eitherToErr eitherErrVal `shouldThrow` (== expected)


--START:testCreateOutputString
test_createOutputString :: Spec
test_createOutputString = do
  context "when called with a single filename" $
    it "returns the right string" $ property $
      \fname' ->
        let fname = fname' :: String
            expected = Printf.printf "The filename is: \"%s\"" fname
        in HCat.createOutputString [fname] `shouldBe` expected
  context "when called with several filenames" $
    it "just uses the first filename" $ property $
      \fnames ->
        let fname = (head fnames) :: String
            expected = Printf.printf "The filename is: \"%s\"" fname
        in HCat.createOutputString [fname] `shouldBe` expected
  context "when called with no filenames" $
    it "returns an error string" $
      HCat.createOutputString [] `shouldBe` "you didn't provide any filenames"
--END:testCreateOutputString
