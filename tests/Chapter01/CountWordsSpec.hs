module Chapter01.CountWordsSpec(spec) where

import Test.Hspec
-- import qualified Data.Text as T (Text, words, pack, sort)
import Data.List(group, sort, sortBy)


countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: String -> [(String, Int)]
countOccurrencesOfWords s =
  fmap tup $ (group . sort . words) s
  where tup e = (head e, length e)

stats :: String -> [String]
stats s = reverse (fmap fst ordered)
  where ordered = sortBy (\a b -> compare (snd a) (snd b)) (countOccurrencesOfWords s) 

spec :: Spec
spec = do
  it "counts the words in a string" $ do
    countWords "one two three, four five-cinque (six)" `shouldBe` 6

  it "counts the occurrences of words" $ do
    countOccurrencesOfWords "one two one one two" `shouldBe` [("one", 3), ("two", 2)]

  it "sorts the words in order of occurrences" $ do
    stats "first first last first last first second second second first first first" `shouldBe` ["first", "second", "last"]