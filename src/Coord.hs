module Coord
  (
  -- * Coordinates
    Coord(C), up, down, left, right, origin
  , cardinalNeighbors
  , addCoord
  ) where

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

up, down, left, right :: Coord -> Coord
up    (C x y) = C x (y-1)
down  (C x y) = C x (y+1)
left  (C x y) = C (x-1) y
right (C x y) = C (x+1) y

origin :: Coord
origin = C 0 0

cardinalNeighbors :: Coord -> [Coord]
cardinalNeighbors (C x y) = [ C (x-1) y, C (x+1) y, C x (y-1), C x (y+1) ]

addCoord :: Coord -> Coord -> Coord
addCoord (C x1 y1) (C x2 y2) = C (x1+x2) (y1+y2)

