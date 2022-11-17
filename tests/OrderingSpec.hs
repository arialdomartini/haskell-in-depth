module OrderingSpec where

import Test.Hspec
import Data.List (sortBy)

sorted :: [Int] -> [Int]
sorted = sortBy (\a b -> if a > b then GT else LT)

spec :: Spec
spec = do
  it "sort an array" $ do
    sorted [5,3,1,2,4] `shouldBe` [1,2,3,4,5]
