module ExtractWordsSpec where

import Test.Hspec ( it, Spec, shouldBe )
import System.Directory ( removeFile )

toWords :: String -> [String]
toWords = words

readWords :: String -> IO [String]
readWords path = do
  content <- readFile path
  return $ toWords content


spec :: Spec
spec = do
  it "extractx all the words from the given text file" $ do
    let path = "text-with-words.txt"
    writeFile path "one two three four five"
    splitWords <- readWords path
    removeFile path

    splitWords `shouldBe` ["one", "two", "three", "four", "five"]
