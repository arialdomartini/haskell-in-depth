module CountWordsSpec where

import Test.Hspec

countWords :: String -> Int
countWords =
  fst . foldr aggregate (0, []) . words
  where aggregate word (count, used) = if word `elem` used then (count, used) else (count+1, word:used)


spec :: Spec
spec = do
  it "counts the number of unique words used (size of the vocabulary)" $ do
    let uniqueWords = countWords "hey joe hey joe have a nice nice day joe" in
      uniqueWords `shouldBe` length ["hey", "joe", "have", "a", "nice", "day"]
