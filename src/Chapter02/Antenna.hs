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


data Direction =
    Clockwise
  | CounterClockwise
  | NoDirection
  | TurnAround
  deriving (Show, Eq)

data Antenna = Antenna Position
  deriving (Show, Eq)

type StartingPosition = Position
type FinalPosition = Position


rotateP :: Direction -> Position -> Position
rotateP NoDirection = id
rotateP Clockwise = csucc
rotateP CounterClockwise = cpred
rotateP TurnAround = csucc . csucc


rotate :: Antenna -> Direction -> Antenna
rotate (Antenna p) m = Antenna (rotateP m p)


rotateMany :: Antenna -> [Direction] -> Antenna
rotateMany = foldl (\ant m -> rotate ant m)
