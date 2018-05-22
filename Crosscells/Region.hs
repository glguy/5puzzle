module Crosscells.Region where

data Coord = Coord Int Int -- ^ row col
  deriving (Read, Show, Eq, Ord)

data Region = Region Coord Coord
  deriving (Read, Show, Eq, Ord)

data Direction = U | D | L | R
  deriving (Read, Show, Eq, Ord)

coordRow, coordCol :: Coord -> Int
coordRow (Coord r _) = r
coordCol (Coord _ c) = c

flipDirection :: Direction -> Direction
flipDirection U = D
flipDirection D = U
flipDirection L = R
flipDirection R = L

pointsTo :: Direction -> Coord -> Region -> Bool
pointsTo U pt reg = sameCol pt reg && above pt (topLeft reg)
pointsTo D pt reg = sameCol pt reg && above (topLeft reg) pt
pointsTo L pt reg = sameRow pt reg && leftOf pt (topLeft reg)
pointsTo R pt reg = sameRow pt reg && leftOf (topLeft reg) pt

sameCol, sameRow :: Coord -> Region -> Bool
sameRow (Coord r _) (Region (Coord r1 _) (Coord r2 _)) = r1 <= r && r <= r2
sameCol (Coord _ c) (Region (Coord _ c1) (Coord _ c2)) = c1 <= c && c <= c2

above, leftOf :: Coord -> Coord -> Bool
above   (Coord r1 _) (Coord r2 _) = r1 < r2
leftOf  (Coord _ c1) (Coord _ c2) = c1 < c2

topLeft :: Region -> Coord
topLeft (Region x _) = x

contained :: Region -> Coord -> Bool
contained (Region (Coord r1 c1) (Coord r2 c2)) (Coord r c) =
  r1 < r && r < r2 && c1 < c && c < c2
