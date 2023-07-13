{-# LANGUAGE DeriveAnyClass #-}

module Chapter02.Antenna where

class (Bounded a, Eq a, Enum a) => CyclicEnum a where
  csucc :: a -> a
  csucc a
    | a == maxBound =  minBound
    | otherwise = succ a

  cpred :: a -> a
  cpred a
    | a == minBound = maxBound
    | otherwise = pred a
  

data Position =
    North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded, CyclicEnum)


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

rotate (Antenna p) Clockwise = Antenna (csucc p)
rotate (Antenna p) CounterClockwise = Antenna (cpred p)
rotate (Antenna p) TurnAround = Antenna (csucc (csucc p))



rotateMany :: Antenna -> [Move] -> Antenna
rotateMany = foldl (\ant m -> rotate ant m)
