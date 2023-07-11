module Chapter01.CountWordsSpec(spec) where

import Test.Hspec
import qualified Data.Text as T (Text, pack, unpack)
import Data.List(group, sort, sortBy)
import qualified Data.Text.IO as F
import System.Directory

countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: String -> [(String, Int)]
countOccurrencesOfWords s =
  fmap tup $ (group . sort . words) s
  where tup e = (head e, length e)

stats :: String -> [String]
stats s = reverse (fmap fst ordered)
  where ordered = sortBy (\a b -> compare (snd a) (snd b)) (countOccurrencesOfWords s) 


-- statsFromFile :: String -> IO ([String])
statsFromFile path = do
  text <- F.readFile path
  let s = stats (T.unpack text)
  return s



spec :: Spec
spec = do
  it "counts the words in a string" $ do
    countWords "one two three, four five-cinque (six)" `shouldBe` 6

  it "counts the occurrences of words" $ do
    countOccurrencesOfWords "one two one one two" `shouldBe` [("one", 3), ("two", 2)]

  it "sorts the words in order of occurrences" $ do
    stats "first first last first last first second second second first first first" `shouldBe` ["first", "second", "last"]

  
  it "works on a real text file" $ do
    F.writeFile path text
    s <- statsFromFile path

    s `shouldBe` ["one", "two", "three"]

    System.Directory.removeFile path
    where path = "count-words-test-file.txt"
          text = T.pack "two two two one three one one one one one one one"
