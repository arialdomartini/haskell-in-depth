module OrderingSpec where

import Test.Hspec
import Data.List (sortBy)

compareNumbers :: Int -> Int -> Ordering
compareNumbers a b = if a > b then GT else LT

sorted :: [Int] -> [Int]
sorted = sortBy compareNumbers

spec :: Spec
spec = do
  it "sorts an array" $ do
    sorted [5,3,1,2,4] `shouldBe` [1,2,3,4,5]
