-- This module solves a 9-piece jigsaw puzzle:
--
-- http://www.alexbrands.com/product/games/one-tough-puzzle/
--
-- The solution is a 3x3 arrangement of the pieces and the edges
-- of the puzzle are not smooth. The pieces can be rotated but
-- not flipped over.
module Main where

import Control.Applicative (liftA2)
import Data.Traversable (for)
import Data.List (tails)
import Linear (V2(V2))
import Prelude hiding (and,any,not,or,all,(&&),(||))

import Ersatz

import Booleans
import FromBit
import Select
import SparseMap

------------------------------------------------------------------------
-- Problem representation
------------------------------------------------------------------------

data Suit = Heart | Spade | Club | Diamond deriving (Eq, Show)
data Parity = Out | In                     deriving (Eq, Show)
data Side = Side Parity Suit               deriving (Eq, Show)


type Piece = [ Side ]

pieces :: [Piece]
pieces =
  [ piece Spade Spade Heart Club
  , piece Heart Diamond Diamond Heart
  , piece Spade Diamond Heart Diamond
  , piece Club Heart Spade Heart
  , piece Club Heart Diamond Club
  , piece Heart Diamond Club Club
  , piece Diamond Club Club Diamond
  , piece Spade Diamond Spade Heart
  , piece Heart Spade Spade Club
  ]
  where
  piece w x y z = [Side Out w, Side Out x, Side In y, Side In z]

rotations :: Piece -> [Piece]
rotations = map (take 4) . take 4 . tails . cycle

locations :: [V2 Int]
locations = liftA2 V2 [0,2,4] [0,2,4]

-- Only the interesting/overlapping edges are mentioned
edges :: [V2 Int]
edges = [ {-==-} V2 0 1, {-==-}  V2 0 3 {-==-}
        , V2 1 0       , V2 1 2,        V2 1 4
        , {-==-} V2 2 1, {-==-}  V2 2 3 {-==-}
        , V2 3 0       , V2 3 2,        V2 3 4
        , {-==-} V2 4 1, {-==-}  V2 4 3 {-==-}
        ]

-- Coordinates of edge cells touching the given coordinate
edgeCoords :: V2 Int -> [V2 Int]
edgeCoords (V2 row col) =
  [V2 (row-1) col, V2 row (col+1), V2 (row+1) col, V2 row (col-1)]

------------------------------------------------------------------------
-- Problem encoding
------------------------------------------------------------------------

-- This encoding is chosen because matching in/out
-- pairs of the same suits will neatly xor to 'true'
encodeSide :: Side -> Bit3
encodeSide (Side x y) =
  case x of
    In  -> not
    Out -> id
  $
  case y of
    Heart   -> Bit3 false false false
    Club    -> Bit3 false false true
    Spade   -> Bit3 false true  false
    Diamond -> Bit3 false true  true

encodePlacementSelect :: Select (V2 Int, Piece) -> SparseMap (V2 Int) Bit3
encodePlacementSelect =
  runSelectWith $ \(loc, sides) ->
       foldr (uncurry SparseMap.insert) false
     $ zip (edgeCoords loc)
     $ map encodeSide sides

encodeCenters :: Select (V2 Int) -> SparseMap (V2 Int) Bit
encodeCenters = runSelectWith $ \loc -> trueList [loc]

------------------------------------------------------------------------
-- Problem predicate
------------------------------------------------------------------------

-- Assignments of puzzles pieces are valid when all 9 cells are
-- covered exactly once, each interesting edge bit is covered exactly once,
-- and all other locations are ignored.
validatePlacements :: [(Select (V2 Int), Select Piece)] -> Bit
validatePlacements xs = isTrue valid
  where
  validEdges   = exactlyOne (encodePlacementSelect . sequence2 <$> xs)
  validCenters = exactlyOne (encodeCenters         . fst       <$> xs)
  outside      = falseList  (edges ++ locations)

  valid        = fmap isTrue validEdges || validCenters || outside

sequence2 :: Applicative f => (f x, f y) -> f (x, y)
sequence2 (x,y) = liftA2 (,) x y

------------------------------------------------------------------------
-- Solution generation
------------------------------------------------------------------------

assignments :: MonadSAT s m => m [(Select (V2 Int), Select Piece)]
assignments =
  do let p1:ps = pieces
     loc1 <- selectList locations
     xs   <- for ps $ \p ->
               do loc    <- selectList locations
                  piece' <- selectList (rotations p)
                  return (loc, piece')
     -- avoids rotating the first piece
     return ( (loc1, pure p1) : xs )

problem :: MonadSAT s m => m [(Select (V2 Int), Select Piece)]
problem = assignments `checking` validatePlacements

sameSolution :: [(Select (V2 Int), Select Piece)] -> [(V2 Int, Piece)] -> Bit
sameSolution xs ys = and (zipWith aux1 xs ys)
  where
  aux1 (loc1, rot1) (loc2, rot2) = sameEq loc1 loc2 && sameEq rot1 rot2

sameEq :: Eq a => Select a -> a -> Bit
sameEq xs y = runSelectWith (\x -> bool (x == y)) xs


main :: IO ()
main = solutions 1 []

solutions :: Int -> [[(V2 Int, Piece)]] -> IO ()
solutions i prevs =
  do res <- solveWith minisat $
                problem `checking` \xs ->
                   not (any (sameSolution xs) prevs)
     case res of
       (Satisfied, Just sol) -> do print i
                                   putStr (render sol)
                                   solutions (i+1) (sol : prevs)
       _ -> putStrLn "End of solutions"

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------


suitChar :: Suit -> Char
suitChar s = case s of
               Heart   -> '♡'
               Spade   -> '♤'
               Club    -> '♧'
               Diamond -> '♢'

rotationChar :: [Side] -> Char
rotationChar [Side Out _, Side Out _, Side In  _, Side In  _] = '┗'
rotationChar [Side In  _, Side Out _, Side Out _, Side In  _] = '┏'
rotationChar [Side In  _, Side In  _, Side Out _, Side Out _] = '┓'
rotationChar [Side Out _, Side In  _, Side In  _, Side Out _] = '┛'

render :: [(V2 Int, Piece)] -> String
render xs = unlines [ [ index (V2 row col) m | col <- [-1 .. 5] ] | row <- [-1 .. 5] ]
  where
  m = renderMap xs

renderMap :: [(V2 Int, Piece)] -> SparseMap (V2 Int) Char
renderMap pieces = fromList ' ' $
  [ (loc1, suitChar suit)
  | (loc, suits) <- pieces
  , (loc1, Side _ suit) <- zip (edgeCoords loc) suits
  ] ++
  [ (loc, rotationChar suits)
  | (loc, suits) <- pieces
  ]
