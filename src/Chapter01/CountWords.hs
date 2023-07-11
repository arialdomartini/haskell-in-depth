module Chapter01.CountWords(
    statsFromFile
  , countOccurrencesOfWords
  , countWords
  , stats) where

import qualified Data.Text as T (Text, words)
import Data.List(group, sort, sortBy)
import qualified Data.Text.IO as F

countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: T.Text -> [(T.Text, Int)]
countOccurrencesOfWords str =
  fmap tup $ (group . sort . T.words ) str
  where tup e = (head e, length e)


sortByUsage :: Ord b => [(a, b)] -> [(a, b)]
sortByUsage = sortBy (\a b -> compare (snd b) (snd a))

stats :: T.Text -> [T.Text]
stats text = fmap fst ordered
  where ordered = sortByUsage $ countOccurrencesOfWords text
  

statsFromFile :: String -> IO [T.Text]
statsFromFile path = do
  fileContent <- F.readFile path
  let t = stats fileContent
  return t
