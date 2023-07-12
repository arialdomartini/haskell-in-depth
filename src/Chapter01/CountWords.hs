module Chapter01.CountWords where

import qualified Data.Text as T (Text, words)
import Data.List(group, sort, sortBy)
import Data.Ord
import qualified Data.Text.IO as F

newtype TextFilePath = Path String
newtype Vocabulary = Vocabulary [T.Text] deriving (Show, Eq)
type Entry = (T.Text, Int)


countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: T.Text -> [Entry]
countOccurrencesOfWords str =
  fmap buildEntry $ (group . sort . T.words ) str
  where
    buildEntry xs@(x:_) = (x, length xs)
    buildEntry _ = error "unexpected"


frequency :: Entry -> Int
frequency = snd

value :: Entry -> T.Text
value = fst

sortByUsage :: [Entry] -> [Entry]
sortByUsage = sortBy (comparing $ Down . frequency)

stats :: T.Text -> Vocabulary
stats text = Vocabulary $ fmap value ordered
  where ordered = sortByUsage $ countOccurrencesOfWords text
  

statsFromFile :: TextFilePath -> IO Vocabulary
statsFromFile (Path path) = do
  fileContent <- F.readFile path
  let t = stats fileContent
  return t
