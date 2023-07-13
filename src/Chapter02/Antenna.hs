{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter02.Antenna where

import Fmt

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

instance Semigroup Direction where
  NoDirection      <> d                = d
  Clockwise        <> Clockwise        = TurnAround
  CounterClockwise <> CounterClockwise = TurnAround
  TurnAround       <> TurnAround       = NoDirection
  Clockwise        <> CounterClockwise = NoDirection
  Clockwise        <> TurnAround       = CounterClockwise
  CounterClockwise <> TurnAround       = Clockwise
  a                <> b                = b <> a

instance Monoid Direction where
  mempty = NoDirection

instance Buildable Position where
  build North = "N"
  build East  = "E"
  build South = "S"
  build West  = "W"

  
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
-- rotateMany = foldl rotate
rotateMany a ds = rotate a (mconcat ds)


printPositions :: [Position] -> String
printPositions positions = fmt $ "Positions: " +| (unwordsF positions) |+ ""
