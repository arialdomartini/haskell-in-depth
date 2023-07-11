module Chapter01.CountWords where

import qualified Data.Text as T (unpack)
import Data.List(group, sort, sortBy)
import qualified Data.Text.IO as F

countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: String -> [(String, Int)]
countOccurrencesOfWords s =
  fmap tup $ (group . sort . words) s
  where tup e = (head e, length e)

stats :: String -> [String]
stats s = reverse (fmap fst ordered)
  where ordered = sortBy (\a b -> compare (snd a) (snd b)) (countOccurrencesOfWords s) 


statsFromFile :: String -> IO [String]
statsFromFile path = do
  text <- F.readFile path
  let s = stats (T.unpack text)
  return s