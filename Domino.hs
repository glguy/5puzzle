module Main where


import Ersatz
import SparseMap
import Select
import Booleans

import Data.List ((\\), mapAccumL)
import Prelude hiding ((&&), (||), or, not)

import Control.Monad.State (MonadState)
import System.Environment

import Diagrams.Prelude (Diagram, lw, translate, square, ultraThick, scale, fromVertices, mkSizeSpec)
import Diagrams.Backend.SVG (B, renderSVG)
import Linear (V2(V2))
import Linear.Affine (Point(P))

type Coord = V2 Int -- row column

------------------------------------------------------------------------
-- Piece representation
------------------------------------------------------------------------

newtype Piece = Piece [Coord] -- coordinates covered by piece
  deriving Show

pieceBitMap :: Piece -> SparseMap Coord Bit
pieceBitMap (Piece xs) = trueList xs

selectPieceBitMap :: Select Piece -> SparseMap Coord Bit
selectPieceBitMap = runSelectWith pieceBitMap

------------------------------------------------------------------------
-- Board locations and piece possibilities
------------------------------------------------------------------------

boardLocations :: Int -> Int -> [Coord]
boardLocations rows cols =
  [V2 row col | row <- [0..rows-1], col <- [0..cols-1]]
  \\ exceptions
  where
  exceptions  = [ topLeft, bottomRight ]
  topLeft     = V2 0 0
  bottomRight = V2 (rows-1) (cols-1)

boardPieces :: (MonadState s m, HasSAT s) => Int -> Int -> m [Select Piece]
boardPieces rows cols = traverse (select . piecesAt) (boardLocations rows cols)

piecesAt :: Coord -> [Piece]
piecesAt c@(V2 row col) = [ noPiece , horizPiece , vertPiece ]
  where
  noPiece    = Piece []
  vertPiece  = Piece [c, V2 (row+1) col]
  horizPiece = Piece [c, V2 row (col+1)]

------------------------------------------------------------------------
-- Problem definition
------------------------------------------------------------------------

problem :: (MonadState s m, HasSAT s) => Int -> Int -> m [Select Piece]
problem rows cols =
  do pieces <- boardPieces rows cols
     assert (validArrangement rows cols pieces)
     return pieces

validArrangement :: Int -> Int -> [Select Piece] -> Bit
validArrangement rows cols pieces = isTrue validLocations
  where
  boardMask         = falseList (boardLocations rows cols)
  pieceBitMaps      = map selectPieceBitMap pieces
  validLocations    = exactlyOne (boardMask : pieceBitMaps)

------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------

main :: IO ()
main =
  do [rows,cols] <- traverse readIO =<< getArgs
     sol <- solveWith minisat (problem rows cols) :: IO (Result, Maybe [Piece])
     case sol of
       (Unsatisfied, _)     -> putStrLn "No solution"
       (Satisfied, Nothing) -> putStrLn "Panic: unable to parse solution"
       (Satisfied, Just xs) ->
           do let sizeSpec = mkSizeSpec (pure Nothing)
              renderSVG "output.svg" sizeSpec (scale 50 (drawSolution rows cols xs))
              putStrLn "Solution saved to output.svg"


------------------------------------------------------------------------
-- Solution rendering
------------------------------------------------------------------------

drawSolution :: Int -> Int -> [Piece] -> Diagram B
drawSolution rows cols ps = grid `mappend` lines ps
  where
  grid = foldMap (\p -> translate (toPoint p) (square 1))
                 (boardLocations rows cols)

  lines =
    foldMap $ \(Piece xs) ->
       lw ultraThick
     $ fromVertices (P . toPoint <$> xs)

  toPoint :: V2 Int -> V2 Double
  toPoint (V2 row col) = V2 (fromIntegral col) (-fromIntegral row)
