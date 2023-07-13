module Chapter02.AntennaSpec where

import Test.Hspec
import Chapter02.Antenna

data TestCaseDirection = TestCaseDirection
  { startingPosition :: StartingPosition
  , rotations            :: [Direction]
  , finalPosition    :: FinalPosition
  }

testCases :: [TestCaseDirection]
testCases =
  [ TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, Clockwise], finalPosition= North}
  , TestCaseDirection { startingPosition= North, rotations= [Clockwise, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, rotations= [NoDirection, NoDirection], finalPosition= North}
  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, CounterClockwise], finalPosition= South}

  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, CounterClockwise, CounterClockwise], finalPosition= East}
  , TestCaseDirection { startingPosition= North, rotations= [Clockwise, Clockwise, Clockwise], finalPosition= West}

  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, NoDirection, Clockwise], finalPosition= North}
  , TestCaseDirection { startingPosition= North, rotations= [NoDirection, NoDirection, NoDirection, Clockwise, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, CounterClockwise, NoDirection], finalPosition= South}
  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, NoDirection, CounterClockwise, CounterClockwise], finalPosition= East}

  
  , TestCaseDirection { startingPosition= North, rotations= [CounterClockwise, TurnAround, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, rotations= [TurnAround, Clockwise], finalPosition= West}
  ]

spec :: Spec
spec = do
  it "move antenna" $ do
    let doTest testCase =
          let antenna = Antenna (startingPosition testCase)
              antenna' = rotateMany antenna (rotations testCase)
              (Antenna position') = antenna' in
            position' `shouldBe` (finalPosition testCase) in
      mapM_ doTest testCases

  it "uses fmt to print positions" $ do
    (printPositions [North, North, West, North, East]) `shouldBe` "Positions: N N W N E"
