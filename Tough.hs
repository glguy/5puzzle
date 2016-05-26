-- This module solves a 9-piece jigsaw puzzle:
--
-- http://www.alexbrands.com/product/games/one-tough-puzzle/
--
-- The solution is a 3x3 arrangement of the pieces and the edges
-- of the puzzle are not smooth. The pieces can be rotated but
-- not flipped over.
module Main where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Traversable (for)
import Data.Foldable (toList)
import Data.List (tails)
import Linear (V2(V2))
import           Data.Map (Map)
import qualified Data.Map as Map
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

rotateList :: Int -> [a] -> [a]
rotateList i xs = zipWith const (drop i (cycle xs)) xs

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

encodePlacementSelect ::
  V2 Int -> Select Piece -> SparseMap (V2 Int) Bit3
encodePlacementSelect loc
  = runSelectWith
  $ fromList false
  . zip (edgeCoords loc)
  . map encodeSide

------------------------------------------------------------------------
-- Solution generation
------------------------------------------------------------------------

assignments :: MonadSAT s m => m (Map (V2 Int) (Select Int, Select Piece))
assignments =
  do ps   <- selectPermutation pieces
     rots <- replicateM 9 (selectList [0..3])
     return (Map.fromList (zip locations (zip rots ps)))

problem :: MonadSAT s m => m (Map (V2 Int) (Select Piece))
problem =
  do xs <- assignments
     let xs' = uncurry (liftA2 rotateList) <$> xs
     assert $ isTrue
            $ trueList edges ==>
              exactlyOne (Map.mapWithKey encodePlacementSelect xs')
     return xs'

main :: IO ()
main =
  do res <- solveWith minisat problem
     case res of
       (Satisfied, Just sol) -> putStr (render (Map.toList sol))
       _                     -> putStrLn "End of solutions"

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
