{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module OrderingSpec where

import Test.Hspec
import Data.List (sortBy)
import GHC.OldList (sort)
import GHC.Base (compareInt)

sorted :: [Int] -> [Int]
sorted = sortBy compareInt


newtype Down' a = Down' a deriving (Eq, Show)
instance Ord a => Ord (Down' a) where
  compare :: Ord a => Down' a -> Down' a -> Ordering
  compare (Down' a) (Down' b)
   | a > b     = LT
   | a < b     = GT
   | otherwise = EQ


spec :: Spec
spec = do
  it "sorts an array" $ do
    sorted [5,3,1,2,4] `shouldBe` [1,2,3,4,5]

  it "sorts an array in descending order" $ do
    sort (fmap Down' [5,3,1,2,4]) `shouldBe` fmap Down' [5,4,3,2,1]
