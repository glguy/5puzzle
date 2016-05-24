-- This module solves a 9-piece jigsaw puzzle:
--
-- http://www.alexbrands.com/product/games/one-tough-puzzle/
--
-- The solution is a 3x3 arrangement of the pieces and the edges
-- of the puzzle are not smooth. The pieces can be rotated but
-- not flipped over.
module Main where

import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Traversable (for)
import Data.List (tails)
import Linear (V2(V2))
import Prelude hiding (not,or,all,(&&),(||))

import Ersatz

import Booleans
import FromBit
import Select
import SparseMap

------------------------------------------------------------------------
-- Problem representation
------------------------------------------------------------------------

data Suit = Heart | Spade | Club | Diamond deriving Show
data Parity = Out | In                     deriving Show
data Side = Side Parity Suit               deriving Show

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
        , V2 3 0       , V2 3 2,        V2 1 4
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

problem :: (MonadState s m, HasSAT s) => m [(Select (V2 Int), Select Piece)]
problem =
  do xs <- for pieces $ \piece ->
              do loc    <- select locations
                 piece' <- select (rotations piece)
                 return (loc, piece')
     assert (validatePlacements xs)
     return xs

main :: IO ()
main =
  do (Satisfied, Just sol) <- solveWith minisat problem
     mapM_ print sol
