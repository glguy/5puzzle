module Palisade.Regions
  (
  -- * Regions
    Region
  , translate
  , inRegion
  , regionCoords
  , sizedRegions
  , symmetries
  , minCoord
  , maxCoord
  , isSolid

  -- * Utility
  , uniques
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable

import Coord

translate :: Coord -> Region -> Region
translate c (Region r) = Region (Set.mapMonotonic (addCoord c) r)

disjoint :: Region -> Region -> Bool
disjoint (Region x) (Region y) = Set.null (Set.intersection x y)

inRegion :: Coord -> Region -> Bool
inRegion c (Region r) = Set.member c r

newtype Region = Region (Set Coord)
  deriving (Read, Show, Eq, Ord)

minCoord :: Region -> Coord
minCoord (Region r) = foldl1 (\(C x1 y1) (C x2 y2) -> C (min x1 x2) (min y1 y2)) r

maxCoord :: Region -> Coord
maxCoord (Region r) = foldl1 (\(C x1 y1) (C x2 y2) -> C (max x1 x2) (max y1 y2)) r

recenter :: Region -> Region
recenter r = translate (C (-x) (-y)) r
  where
    C x y = minCoord r

normalize :: Region -> Region
normalize = minimum . symmetries

mapRegion :: (Coord -> Coord) -> Region -> Region
mapRegion f (Region r) = Region (Set.map f r)

addRegion :: Coord -> Region -> Region
addRegion c (Region r) = Region (Set.insert c r)

regionCoords :: Region -> [Coord]
regionCoords (Region r) = Set.toList r

symmetries :: Region -> [Region]
symmetries r =
  [ recenter (mapRegion (f1 . f2 . f3) r)
  | f1 <- [id, \(C x y) -> C (-x) y]
  , f2 <- [id, \(C x y) -> C x (-y)]
  , f3 <- [id, \(C x y) -> C y x   ]
  ]

grow :: Region -> [Region]
grow r = uniques
  [ normalize (addRegion c r)
  | c <- uniques (cardinalNeighbors =<< regionCoords r)
  , not (inRegion c r) ]

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList

sizedRegions :: Int -> [Region]
sizedRegions n = loop n [Region (Set.singleton origin)]
  where
    loop 1 xs = xs
    loop n xs = loop (n-1) (uniques (concatMap grow xs))

isSolid :: Region -> Bool
isSolid r = search border candidates
  where
    C xlo ylo = minCoord r
    C xhi yhi = maxCoord r
    candidates = Set.fromList
                 [ c | c <- C <$> [xlo..xhi] <*> [ylo..yhi]
                     , not (inRegion c r) ]
    border = [ C xlo y | y <- [ylo..yhi] ]
          ++ [ C xhi y | y <- [ylo..yhi] ]
          ++ [ C x ylo | x <- [xlo..xhi] ]
          ++ [ C x yhi | x <- [xlo..xhi] ]

    search [] candidates = Set.null candidates
    search (x:xs) candidates
      | Set.member x candidates = search (cardinalNeighbors x ++ xs)
                                         (Set.delete x candidates)
      | otherwise = search xs candidates
