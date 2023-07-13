module Chapter02.Antenna where

data Position =
    North
  | East
  | South
  | West
  deriving (Show, Eq)

data Move =
    Clockwise
  | CounterClockwise
  | NoMove
  deriving (Show, Eq)

data Antenna = Antenna Position
  deriving (Show, Eq)

type StartingPosition = Position
type FinalPosition = Position


move :: Antenna -> Move -> Antenna
move (Antenna North) NoMove = Antenna North
move (Antenna East) NoMove = Antenna East
move (Antenna South) NoMove = Antenna South
move (Antenna West) NoMove = Antenna West

move (Antenna North) Clockwise = Antenna East
move (Antenna East) Clockwise = Antenna South
move (Antenna South) Clockwise = Antenna West
move (Antenna West) Clockwise = Antenna North

move (Antenna North) CounterClockwise = Antenna West
move (Antenna East) CounterClockwise = Antenna North
move (Antenna South) CounterClockwise = Antenna East
move (Antenna West) CounterClockwise = Antenna South

