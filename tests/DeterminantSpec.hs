module DeterminantSpec where

import Test.Hspec
import GHC.Float (int2Double)

type Size = Int
type Index = Int
type Value = Double
type Row = [Value]
type Matrix = [Row]

value :: Index -> Index -> Value
value i j = int2Double 1 / int2Double (i + j)

matrix :: Int -> Matrix
matrix size = [ [1 / int2Double (i +j) | j <- [1..size]] | i <- [1..size]]


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
