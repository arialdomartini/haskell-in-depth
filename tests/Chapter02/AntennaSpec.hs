module Chapter02.AntennaSpec where

import Test.Hspec
import Chapter02.Antenna

data TestCaseDirection = TestCaseDirection
  { startingPosition :: StartingPosition
  , moves            :: [Direction]
  , finalPosition    :: FinalPosition
  }

testCases :: [TestCaseDirection]
testCases =
  [ TestCaseDirection { startingPosition= North, moves= [CounterClockwise, Clockwise], finalPosition= North}
  , TestCaseDirection { startingPosition= North, moves= [Clockwise, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, moves= [NoDirection, NoDirection], finalPosition= North}
  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, CounterClockwise], finalPosition= South}

  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, CounterClockwise, CounterClockwise], finalPosition= East}
  , TestCaseDirection { startingPosition= North, moves= [Clockwise, Clockwise, Clockwise], finalPosition= West}

  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, NoDirection, Clockwise], finalPosition= North}
  , TestCaseDirection { startingPosition= North, moves= [NoDirection, NoDirection, NoDirection, Clockwise, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, CounterClockwise, NoDirection], finalPosition= South}
  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, NoDirection, CounterClockwise, CounterClockwise], finalPosition= East}

  
  , TestCaseDirection { startingPosition= North, moves= [CounterClockwise, TurnAround, Clockwise], finalPosition= South}
  , TestCaseDirection { startingPosition= North, moves= [TurnAround, Clockwise], finalPosition= West}
  ]

spec :: Spec
spec = do
  -- move :: StartingPosition -> Direction -> FinalPosition
  it "move antenna" $ do
    mapM_ doTest testCases
    where doTest testCase =
            let antenna = Antenna (startingPosition testCase)
                antenna' = rotateMany antenna (moves testCase)
                (Antenna position') = antenna' in
              position' `shouldBe` (finalPosition testCase)

