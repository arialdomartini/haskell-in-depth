module CountWordsTextSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (group, sort)
import Data.Char (isLetter)
import GHC.OldList (sortOn)
import qualified System.Directory as Directory

text :: T.Text
text = T.pack "hey joe, how are you? I think Joe you are fine, aren't you? Bye Joe! Bye Joe joe!"

path :: FilePath
path = "count-words-text-spec.txt"

elaborate :: T.Text -> (T.Text, Int)
elaborate =
    last
  . sortOn snd
  . fmap (\e -> (head e, length e))
  . group
  . sort
  . fmap (T.toCaseFold . T.dropAround (not . isLetter))
  . T.words

getMostFrequentWord :: FilePath -> IO (T.Text, Int)
getMostFrequentWord filePath =
  elaborate <$> TIO.readFile filePath

spec :: Spec
spec = do
  it "finds the most frequent word in a text file" $ do
    TIO.writeFile path text
    mostFrequent <- getMostFrequentWord path
    mostFrequent `shouldBe` (T.pack "joe", 5)
    Directory.removeFile path
