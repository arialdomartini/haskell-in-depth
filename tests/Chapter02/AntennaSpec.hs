module Chapter02.AntennaSpec where

import Test.Hspec
import Chapter02.Antenna

data TestCaseMove = TestCaseMove
  { startingPosition :: StartingPosition
  , moves            :: [Move]
  , finalPosition    :: FinalPosition
  }

testCases :: [TestCaseMove]
testCases =
  [ TestCaseMove { startingPosition= North, moves= [CounterClockwise, Clockwise], finalPosition= North}
  , TestCaseMove { startingPosition= North, moves= [Clockwise, Clockwise], finalPosition= South}
  , TestCaseMove { startingPosition= North, moves= [NoMove, NoMove], finalPosition= North}
  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, CounterClockwise], finalPosition= South}

  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, CounterClockwise, CounterClockwise], finalPosition= East}
  , TestCaseMove { startingPosition= North, moves= [Clockwise, Clockwise, Clockwise], finalPosition= West}

  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, NoMove, Clockwise], finalPosition= North}
  , TestCaseMove { startingPosition= North, moves= [NoMove, NoMove, NoMove, Clockwise, Clockwise], finalPosition= South}
  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, CounterClockwise, NoMove], finalPosition= South}
  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, NoMove, CounterClockwise, CounterClockwise], finalPosition= East}

  
  , TestCaseMove { startingPosition= North, moves= [CounterClockwise, TurnAround, Clockwise], finalPosition= South}
  , TestCaseMove { startingPosition= North, moves= [TurnAround, Clockwise], finalPosition= West}
  ]

spec :: Spec
spec = do
  -- move :: StartingPosition -> Move -> FinalPosition
  it "move antenna" $ do
    mapM_ doTest testCases
    where doTest testCase =
            let antenna = Antenna (startingPosition testCase)
                antenna' = foldl (\ant m -> rotate ant m) antenna (moves testCase)
                (Antenna position') = antenna' in
              position' `shouldBe` (finalPosition testCase)

