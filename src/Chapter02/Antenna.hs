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
  | TurnAround
  deriving (Show, Eq)

data Antenna = Antenna Position
  deriving (Show, Eq)

type StartingPosition = Position
type FinalPosition = Position


rotate :: Antenna -> Move -> Antenna
rotate (Antenna North) NoMove = Antenna North
rotate (Antenna East) NoMove = Antenna East
rotate (Antenna South) NoMove = Antenna South
rotate (Antenna West) NoMove = Antenna West

rotate (Antenna North) Clockwise = Antenna East
rotate (Antenna East) Clockwise = Antenna South
rotate (Antenna South) Clockwise = Antenna West
rotate (Antenna West) Clockwise = Antenna North

rotate (Antenna North) CounterClockwise = Antenna West
rotate (Antenna East) CounterClockwise = Antenna North
rotate (Antenna South) CounterClockwise = Antenna East
rotate (Antenna West) CounterClockwise = Antenna South


rotate (Antenna North) TurnAround = Antenna South
rotate (Antenna East) TurnAround = Antenna West
rotate (Antenna South) TurnAround = Antenna North
rotate (Antenna West) TurnAround = Antenna East


rotateMany :: Antenna -> [Move] -> Antenna
rotateMany = foldl (\ant m -> rotate ant m)
