module Chapter01.CountWords where

import qualified Data.Text as T (Text, words)
import Data.List(group, sort)
import Data.Ord
import qualified Data.Text.IO as F

newtype TextFilePath = Path String
newtype Vocabulary = Vocabulary [T.Text] deriving (Show, Eq)
newtype Entry = Entry (T.Text, Int) deriving (Show, Eq)


frequency :: Entry -> Int
frequency (Entry (_, f)) = f

value :: Entry -> T.Text
value (Entry (v, _)) = v

instance Ord Entry where
  compare = comparing (Down . frequency)


countWords :: String -> Int
countWords = length . words

countOccurrencesOfWords :: T.Text -> [Entry]
countOccurrencesOfWords str =
  fmap buildEntry $ (group . sort . T.words ) str
  where
    buildEntry xs@(x:_) = Entry (x, length xs)
    buildEntry _ = error "unexpected"


sortByUsage :: [Entry] -> [Entry]
sortByUsage = sort

stats :: T.Text -> Vocabulary
stats text = Vocabulary $ fmap value ordered
  where ordered = sortByUsage $ countOccurrencesOfWords text
  

statsFromFile :: TextFilePath -> IO Vocabulary
statsFromFile (Path path) = do
  fileContent <- F.readFile path
  let t = stats fileContent
  return t
