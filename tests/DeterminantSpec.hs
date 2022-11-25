{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module DeterminantSpec where

import Test.Hspec
import GHC.Float (int2Double)
import Control.Exception (evaluate)

type Step = Int
type Size = Int
type Index = Int
type Value = Double
type Row = [Value]
type Matrix = [Row]

value :: Index -> Index -> Value
value i j = int2Double 1 / int2Double (i + j)

matrix :: Int -> Matrix
matrix size = [ [1 / int2Double (i +j) | j <- [1..size]] | i <- [1..size]]

determinant :: Matrix -> Value
determinant ((0:_):_) = error "A[0,0] == 0: Shuffling the matrix is still unsupported"
determinant _ = undefined

size :: Matrix -> Size
size = length

indexesOf :: Matrix -> [Index]
indexesOf m = [0..size m - 1]

modify :: Index -> Row -> Matrix -> Matrix
modify i row m = fmap modifyRow (indexesOf m) where
  modifyRow index
    | index == i = row
    | otherwise = m !! index

(*^) :: Double -> Row -> Row
f *^ row = fmap (f *) row

(-^) :: Row -> Row -> Row
--[]     -^  []    = []
--(a:as) -^ (b:bs) = (a - b : as -^ bs)
(-^) = zipWith (-)


step :: Matrix -> Step -> Matrix
step m step = fmap modifiedRow (indexesOf m) where

  modifiedRow :: Index -> Row
  modifiedRow index
    | index <= step = m !! index
    | otherwise = row -^ (factor *^ row0) where
        factor = (m !! index !! step) / (row0 !! step)
        row = m !! index
        row0  = m !! step

allSteps :: Matrix -> Matrix
allSteps m = foldl step m (indexesOf m)



spec :: Spec
spec = do
  it "should fail" $ do
    matrix 7 `shouldBe`
      [  [1/(1+1), 1/(2+1), 1/(3+1), 1/(4+1), 1/(5+1), 1/(6+1), 1/(7+1)]
        ,[1/(1+2), 1/(2+2), 1/(3+2), 1/(4+2), 1/(5+2), 1/(6+2), 1/(7+2)]
        ,[1/(1+3), 1/(2+3), 1/(3+3), 1/(4+3), 1/(5+3), 1/(6+3), 1/(7+3)]
        ,[1/(1+4), 1/(2+4), 1/(3+4), 1/(4+4), 1/(5+4), 1/(6+4), 1/(7+4)]
        ,[1/(1+5), 1/(2+5), 1/(3+5), 1/(4+5), 1/(5+5), 1/(6+5), 1/(7+5)]
        ,[1/(1+6), 1/(2+6), 1/(3+6), 1/(4+6), 1/(5+6), 1/(6+6), 1/(7+6)]
        ,[1/(1+7), 1/(2+7), 1/(3+7), 1/(4+7), 1/(5+7), 1/(6+7), 1/(7+7)]]


  it "fails is A[1,1] == 0" $ do
     let m = [[0]] in
       evaluate (determinant m) `shouldThrow` (errorCall "A[0,0] == 0: Shuffling the matrix is still unsupported")

  it "modifies a row" $ do
    let m3 =        [[11,21,22]
                   ,[21,22,23]
                   ,[31,32,33]]
        expected = [[11,21,22]
                   ,[99,99,99]
                   ,[31,32,33]]

      in modify 1 [99,99,99] m3 `shouldBe` expected


  it "subtracts 2 rows" $ do
    [ 10, 20, 30] -^
     [1,  2,  3 ] `shouldBe`
     [9,  18, 27]


  it "applies the step 1 to a matrix" $ do
    let m3 =       [[11,12,13]
                   ,[21,22,23]
                   ,[31,32,33]]
        expected = [[11,12,13]
                   ,[21 - 21/11 * 11, 22 - 21/11 * 12, 23 - 21/11 * 13]
                   ,[31 - 31/11 * 11, 32 - 31/11 * 12, 33 - 31/11 * 13]]
      in step m3 0 `shouldBe` expected

  it "applies all the steps to a matrix" $ do
    let m3 =       [[11,12,13]
                   ,[21,22,23]
                   ,[31,32,33]]
        expected = [[11,12,13]
                   ,[21 - 21/11 * 11, 22 - 21/11 * 12, 23 - 21/11 * 13]
                   ,[31 - 31/11 * 11, 32 - 31/11 * 12, 33 - 31/11 * 13 + 999]]
      in allSteps m3 `shouldBe` expected
