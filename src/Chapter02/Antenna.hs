module Chapter02.Antenna where

data Position =
    North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded)


data Move =
    Clockwise
  | CounterClockwise
  | NoMove
  | TurnAround
  deriving (Show, Eq)

data Antenna = Antenna Position
  deriving (Show, Eq)

type StartingPosition = Position
type FinalPosition = Position


rotate :: Antenna -> Move -> Antenna

rotate a NoMove = a

rotate (Antenna p) Clockwise =
  if p == maxBound
  then Antenna minBound
  else Antenna (succ p)

rotate (Antenna p) CounterClockwise =
  if p == minBound
  then Antenna maxBound
  else Antenna (pred p)

rotate a TurnAround = rotate (rotate a Clockwise) Clockwise



rotateMany :: Antenna -> [Move] -> Antenna
rotateMany = foldl (\ant m -> rotate ant m)
