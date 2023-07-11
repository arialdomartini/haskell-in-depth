module Chapter01.CountWordsSpec(spec) where

import Test.Hspec
import qualified Data.Text as T (words, pack)

countWords :: String -> Int
countWords = length . T.words . T.pack

spec :: Spec
spec = do
  it "counts the words in a string" $ do
    countWords "one two three, four five-cinque (six)" `shouldBe` 6