{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module ListTripleSpec where

import Test.Hspec
import Data.List (groupBy)


orig     = [ 1,2,3,  4,5,6,  100,200,300]
expected = [[1,2,3],[4,5,6],[100,200,300]]

group3 :: [a] -> [[a]]
group3 [] = []
group3 (a:b:c:rest) = [a,b,c] : group3 rest

group3Zip l = (fmap . fmap) fst grouped
  where grouped = groupBy (\(_,i) (_,j) -> i == j) zipped
        zipped = zip l (fmap (`div` 3) indexes)
        indexes = [0..length l - 1]

group3Fold l = result
  where (_, _, result) = foldl groupItem (0, [], []) l
        groupItem (i, el, acc) e =
            if i < 2
            then (i+1, el ++ [e], acc)
            else (0,   [], acc ++ [el++[e]])

group3Fold' l = snd $ fmap (fmap reverse . reverse) (foldl groupItem ([], []) l)
  where
        groupItem (el, acc) e =
            if length el < 2
            then (e : el, acc)
            else ([], (e:el) : acc)



spec :: Spec
spec = do
  it "groups lists using recursive a function" $ do
    group3 orig `shouldBe` expected

  it "groups lists using zip and group" $ do
    group3Zip orig `shouldBe` expected

  it "groups lists using zip and group" $ do
    group3Fold orig `shouldBe` expected

  it "groups lists using zip and group, alternative" $ do
    group3Fold' orig `shouldBe` expected
