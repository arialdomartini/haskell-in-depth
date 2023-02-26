module AntennaSpec where

import Test.Hspec

data Direction =
  North | East | South | West
  deriving (Eq, Show, Enum, Bounded)

data Turn =
  DoNotTurn| TurnLeft | TurnRight | TurnAround
  deriving (Eq, Show, Enum, Bounded)

rotate :: Direction -> Turn -> Direction
orient :: Direction -> Direction -> Turn

rotateMany :: Direction -> [Turn] -> Direction
rotateManySteps :: Direction -> [Turn] -> [Direction]
--orientMany :: [Direction] -> [Turn]

--rotateFromFile :: Direction -> FilePath -> IO()
--orientFromFile :: FilePath -> IO()

rotate d TurnRight =
  if d == maxBound
  then minBound
  else succ d
rotate d TurnLeft =
  if d == minBound
  then maxBound
  else pred d
rotate d DoNotTurn = d
rotate d TurnAround = rotate (rotate d TurnLeft) TurnLeft

orient North North = DoNotTurn
orient North East  = TurnRight
orient North South = TurnAround
orient North West  = TurnLeft

orient South North = TurnAround
orient South East  = TurnRight
orient South South = DoNotTurn
orient South West  = TurnLeft

orient West North = TurnRight
orient West East  = TurnAround
orient West South = TurnLeft
orient West West  = DoNotTurn

orient East North = TurnLeft
orient East East  = DoNotTurn
orient East South = TurnRight
orient East West  = TurnAround


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
