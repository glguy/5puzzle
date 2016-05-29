{-# Language TypeFamilies #-}
{-# Language PackageImports #-}
module Main where

import "base"         Data.List (sort, nub)
import "base"         Control.Applicative (liftA2)
import "base"         System.Environment (getArgs)
import                Prelude hiding (not, or, and, all, (&&), (||))

import "colour"       Data.Colour.Names
import "diagrams-lib" Diagrams.Prelude (Diagram, Colour, mkSizeSpec, scale, translate, fc, square)
import "diagrams-svg" Diagrams.Backend.SVG (B, renderSVG)
import "ersatz"       Ersatz
import "linear"       Linear (V2(V2))
import "transformers" Control.Monad.Trans.State (StateT)
import "split"        Data.List.Split (splitWhen)

import Select
import SparseMap
import Booleans

newtype Piece = Piece [V2 Int]
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Piece parsing and loading

loadPieces :: FilePath -> IO [Piece]
loadPieces fn =
  do txt <- readFile fn
     let rawPieces = splitWhen null (lines txt)
     return (map parsePiece rawPieces)

parsePiece :: [String] -> Piece
parsePiece xs = Piece [ V2 rowIx colIx
                        | (rowIx, row) <- zip [0..] xs
                        , (colIx, '*') <- zip [0..] row
                        ]

------------------------------------------------------------------------
-- Piece manipulation

translatePiece :: V2 Int -> Piece -> Piece
translatePiece o (Piece xs) = Piece ((+ o) <$> xs)

-- | Translate a piece such that it's minimum row and column
-- are at index 0.
retranslate :: Piece -> Piece
retranslate (Piece []) = Piece []
retranslate (Piece xs) = translatePiece (negate o) (Piece xs)
  where
  o = foldl1 (liftA2 min) xs

-- | Arrange the coördinates of a piece in a sorted order to
-- make the pieces suitable for comparison with each other.
normalizePiece :: Piece -> Piece
normalizePiece (Piece xs) = Piece (sort xs)

-- | Find all possible positions that a given piece could fit on an
-- empty board.
placements :: [V2 Int] -> Piece -> [Piece]
placements board p = translatePiece <$> offsets <*> orientations p
  where
  offsets = liftA2 V2 [xlo .. xhi] [ylo .. yhi]
  V2 xlo ylo = foldl1 (liftA2 min) board
  V2 xhi yhi = foldl1 (liftA2 max) board

-- | Given a piece return a list of the unique ways it can be rotated
-- and flipped.
orientations :: Piece -> [Piece]
orientations (Piece xs) =
  nub [ normalizePiece (retranslate p)
      | f <- [id, \(V2 x y) -> V2 y x]
      , g <- [id, \(V2 x y) -> V2 (-x) y]
      , h <- [id, \(V2 x y) -> V2 x (-y)]
      , let p = Piece (h . g . f <$> xs)
      ]

------------------------------------------------------------------------
-- Board representation

type Board = SparseMap (V2 Int) Bit

------------------------------------------------------------------------
-- Piece to board operations


-- | Given a bit indicating if this piece is active construct a board
-- representing the locations covered by this piece.
pieceToBits :: Piece -> Board
pieceToBits (Piece xs) = trueList xs

-- | Construct a board representing the locations covered by the selected
-- piece in a set of possible choices.
selectPieceMask :: Select Piece -> Board
selectPieceMask = runSelectWith pieceToBits


------------------------------------------------------------------------
-- Main program logic

type M = StateT SAT IO

main :: IO ()
main =
  do args <- getArgs
     let fn = case args of
                []  -> "pieces.txt"
                x:_ -> x
     Piece board:pieces <- loadPieces fn
     (Satisfied, Just sol) <- solveWith minisat (problem board pieces)
     let sizeSpec = mkSizeSpec (pure Nothing)
     renderSVG "output.svg" sizeSpec (drawSolution sol)
     putStrLn "Solution saved to output.svg"

-- | Select an arrangement of the pieces that satisfies the covering
-- predicate for this puzzle.
problem :: [V2 Int] -> [Piece] -> M [Select Piece]
problem board pieces = traverse (selectList . placements board) pieces
            `checking` choicePredicate board

-- | A choice is valid when every location on the board is covered exactly once
choicePredicate :: [V2 Int] -> [Select Piece] -> Bit
choicePredicate board choices = validPositions
  where
  boardMask    = trueList board
  pieceBitMaps = map selectPieceMask choices
  validPositions = coverOne boardMask pieceBitMaps


------------------------------------------------------------------------
-- Solution renderer

drawSolution :: [Piece] -> Diagram B
drawSolution = scale 50 . foldMap (uncurry drawPiece) . zip palette

drawPiece :: Colour Double -> Piece -> Diagram B
drawPiece c (Piece xs) = foldMap (drawSquare c) xs

drawSquare :: Colour Double -> V2 Int -> Diagram B
drawSquare c o = translate (convertCoordinate o)
               $ fc c
               $ square 1

convertCoordinate :: V2 Int -> V2 Double
convertCoordinate (V2 row col) = V2 (fromIntegral col) (- fromIntegral row)

palette :: [Colour Double]
palette = cycle [red, yellow, blue, brown, black, green,
           cyan, salmon, orange, gold, gray, pink]
