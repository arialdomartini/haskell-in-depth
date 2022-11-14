module CountWordsSpec where

import Test.Hspec
import Data.List (sortOn, group)
import Data.Char (isLetter)
import GHC.OldList (sort)

countWords :: String -> Int
countWords =
  fst . foldr aggregate (0, []) . words
  where aggregate word (count, used) = if word `elem` used then (count, used) else (count+1, word:used)

updateFrequency :: [(String, Int)] -> String -> [(String, Int)]
updateFrequency ((w, f):rest) word =
  if w == word
  then (w, f+1) : rest
  else (w, f)   : updateFrequency rest word
updateFrequency []  w = [(w, 1)]



mostFrequentWord :: String -> (String, Int)
mostFrequentWord =
  last . sortOn snd . foldr aggregate [] . words
  where aggregate word frequencies = updateFrequency frequencies word


removeNonLetters :: String -> String
removeNonLetters = takeWhile isLetter . dropWhile  (not . isLetter)

mostFrequentWord' :: String -> (String, Int)
mostFrequentWord' = last . sortOn snd . fmap (\e -> (head e, length e)) . group . sort . fmap removeNonLetters . words

spec :: Spec
spec = do
  it "counts the number of unique words used (size of the vocabulary)" $ do
    let uniqueWords = countWords "hey joe hey joe have a nice nice day joe" in
      uniqueWords `shouldBe` length ["hey", "joe", "have", "a", "nice", "day"]

  it "updates frequency" $ do
    updateFrequency [] "joe"  `shouldBe`  [("joe", 1)]
    updateFrequency [("joe", 2), ("mary", 1)] "joe"  `shouldBe`  [("joe", 3), ("mary", 1)]
    updateFrequency [("joe", 2), ("mary", 1)] "mary" `shouldBe`  [("joe", 2), ("mary", 2)]

  it "calculates the most frequent word" $ do
    let word = mostFrequentWord "hey joe joe hey joe have a nice nice day joe" in
      word `shouldBe` ("joe", 4)

  it "calculates the most frequent word, ignoring non letters" $ do
    let word = mostFrequentWord' "hey joe!! joe, hey joe! have a nice (nice) day joe!!" in
      word `shouldBe` ("joe", 4)
