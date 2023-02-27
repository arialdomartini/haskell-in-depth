{-# LANGUAGE DeriveAnyClass #-}
module AntennaSpec where

import Test.Hspec

data Direction =
  North | East | South | West
  deriving (Eq, Show, Enum, Bounded, CyclicEnum)

data Turn =
  DoNotTurn| TurnLeft | TurnRight | TurnAround
  deriving (Eq, Show)

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  cyclicSucc :: a -> a
  cyclicSucc d
   | d == maxBound = minBound
   | otherwise     = succ d

  cyclicPred :: a -> a
  cyclicPred d
   | d == minBound = maxBound
   | otherwise     = pred d

rotate :: Direction -> Turn -> Direction
orient :: Direction -> Direction -> Turn

rotateMany :: Direction -> [Turn] -> Direction
rotateManySteps :: Direction -> [Turn] -> [Direction]
--orientMany :: [Direction] -> [Turn]

--rotateFromFile :: Direction -> FilePath -> IO()
--orientFromFile :: FilePath -> IO()

rotate d TurnRight = cyclicSucc d
rotate d TurnLeft  = cyclicPred d
rotate d DoNotTurn = d
rotate d TurnAround = (cyclicSucc . cyclicSucc) d

orient from to
 | from == to             = DoNotTurn
 | from == cyclicPred to  = TurnRight
 | from == cyclicSucc to  = TurnLeft
 | otherwise              = TurnAround

rotateMany       = foldl rotate
rotateManySteps  = scanl rotate

spec :: Spec
spec = do
  it "rotates an antenna" $ do
    rotate North TurnLeft   `shouldBe` West
    rotate North TurnRight  `shouldBe` East
    rotate East  DoNotTurn  `shouldBe` East
    rotate West  TurnAround `shouldBe` East

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
