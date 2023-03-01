{-# LANGUAGE DeriveAnyClass #-}
module AntennaSpec where

import Test.Hspec

data Direction =
  North | East | South | West
  deriving (Eq, Show, Enum, Bounded, CyclicEnum)

data Turn =
  DoNotTurn| TurnLeft | TurnRight | TurnAround
  deriving (Eq, Show, Enum, Bounded)

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  cyclicSucc :: a -> a
  cyclicSucc d
   | d == maxBound = minBound
   | otherwise     = succ d

  cyclicPred :: a -> a
  cyclicPred d
   | d == minBound = maxBound
   | otherwise     = pred d

rotate :: Turn -> Direction -> Direction
orient :: Direction -> Direction -> Turn

rotateMany :: Direction -> [Turn] -> Direction
rotateManySteps :: Direction -> [Turn] -> [Direction]
--orientMany :: [Direction] -> [Turn]

--rotateFromFile :: Direction -> FilePath -> IO()
--orientFromFile :: FilePath -> IO()

rotate TurnRight = cyclicSucc
rotate TurnLeft  = cyclicPred
rotate DoNotTurn = id
rotate TurnAround = cyclicSucc . cyclicSucc


orient from to = head $ filter (\t -> rotate t from == to) every

every :: (Bounded a, Enum a) => [a]
every = [minBound..maxBound]

rotateMany       = foldl (\a i -> rotate i a)
rotateManySteps  = scanl (\a i -> rotate i a)

spec :: Spec
spec = do
  it "rotates an antenna" $ do
    rotate TurnLeft North   `shouldBe` West
    rotate TurnRight North  `shouldBe` East
    rotate DoNotTurn  East  `shouldBe` East
    rotate TurnAround  West `shouldBe` East

  it "orients an antenna" $ do
    orient North North `shouldBe` DoNotTurn
    orient North South `shouldBe` TurnAround
    orient East  North `shouldBe` TurnLeft
    orient West  North `shouldBe` TurnRight

  it "rotates an antenna in many turns" $ do
    rotateMany North [ TurnLeft, TurnRight ]                      `shouldBe` North
    rotateMany North [ TurnLeft, TurnLeft, TurnLeft]              `shouldBe` East
    rotateMany North [ TurnAround, TurnAround, TurnRight]         `shouldBe` East
    rotateMany South [ TurnAround, TurnRight, TurnLeft, TurnLeft] `shouldBe` West

  it "rotates an antenna in many turns, keeping track of the steps" $ do
    rotateManySteps North [ TurnLeft, TurnRight ]                      `shouldBe` [North, West, North]
    rotateManySteps North [ TurnLeft, TurnLeft, TurnLeft]              `shouldBe` [North, West, South, East]
    rotateManySteps North [ TurnAround, TurnAround, TurnRight]         `shouldBe` [North, South, North, East]
    rotateManySteps South [ TurnAround, TurnRight, TurnLeft, TurnLeft] `shouldBe` [South, North, East, North, West]
