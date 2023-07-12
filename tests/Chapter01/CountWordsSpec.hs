{-# LANGUAGE OverloadedStrings #-}

module Chapter01.CountWordsSpec(spec) where

import Test.Hspec
import qualified Data.Text.IO as F
import System.Directory
import Chapter01.CountWords


spec :: Spec
spec = do
  it "counts the words in a string" $ do
    countWords "one two three, four five-cinque (six)" `shouldBe` 6

  it "counts the occurrences of words" $ do
    countOccurrencesOfWords "one two one one two" `shouldBe` [Entry ("one", 3), Entry ("two", 2)]

  it "sorts the words in order of occurrences" $ do
    stats "first first last first last first second second second first first first" `shouldBe` Vocabulary ["first", "second", "last"]

  
  it "works on a real text file" $ do
    F.writeFile path text
    s <- statsFromFile (Path path)

    s `shouldBe` Vocabulary ["one", "two", "three"]

    System.Directory.removeFile path
    where path = "count-words-test-file.txt"
          text = "two two two one three one one one one one one one"
