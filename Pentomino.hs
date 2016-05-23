{-# Language TypeFamilies #-}
{-# Language PackageImports #-}
module Main where

import "base"         Data.List ((\\), sort, findIndex, nub, mapAccumL)
import "base"         Data.Maybe (fromJust)
import "base"         Control.Applicative (liftA2)
import "base"         Control.Monad (replicateM)
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

newtype Piece = Piece [V2 Int]
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Piece parsing and loading

loadPieces :: IO [Piece]
loadPieces =
  do txt <- readFile "pieces.txt"
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
translatePiece o (Piece xs) = Piece (liftA2 (+) o <$> xs)

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
placements :: Piece -> [Piece]
placements p =
  [ translatePiece (V2 dx dy) p'
  | p' <- orientations p
  , dx <- [0..7]
  , dy <- [0..7]
  ]

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

-- | Set of valid board locations: 8x8 grid with center 2x2 square removed
boardPositions :: [V2 Int]
boardPositions = liftA2 V2 [0..7] [0..7]
              \\ liftA2 V2 [3..4] [3..4]

------------------------------------------------------------------------
-- Piece to board operations


-- | Given a bit indicating if this piece is active construct a board
-- representing the locations covered by this piece.
pieceToBits :: Piece -> Board
pieceToBits (Piece xs) = trueList xs

-- | Construct a board representing the locations covered by the selected
-- piece in a set of possible choices.
selectPieceMask :: Select Piece -> Board
selectPieceMask = foldSelect $ \active o -> constant active && pieceToBits o


------------------------------------------------------------------------
-- Main program logic

type M = StateT SAT IO

main :: IO ()
main =
  do pieces <- loadPieces
     (Satisfied, Just sol) <- solveWith minisat (problem pieces)
     let sizeSpec = mkSizeSpec (pure Nothing)
     renderSVG "output.svg" sizeSpec (drawSolution sol)
     putStrLn "Solution saved to output.svg"

-- | Select an arrangement of the pieces that satisfies the covering
-- predicate for this puzzle.
problem :: [Piece] -> M [Select Piece]
problem pieces =
  do choices <- traverse (select . placements) pieces
     assert (choicePredicate choices)
     return choices

-- | A choice is valid when every location on the board is covered exactly once
choicePredicate :: [Select Piece] -> Bit
choicePredicate choices = true === validPositions
  where
  boardMask    = falseList boardPositions
  pieceBitMaps = map selectPieceMask choices
  validPositions = exactlyOne (boardMask : pieceBitMaps)

-- | Returns a summary value of where a boolean is true in exactly
-- one position in the list.
exactlyOne :: Boolean a => [a] -> a
exactlyOne xs = allCovered && nor overlaps
  where
  (allCovered, overlaps) = mapAccumL addMask false xs

  addMask covered mask = (covered || mask, covered && mask)


------------------------------------------------------------------------
-- Solution renderer

drawSolution :: [Piece] -> Diagram B
drawSolution = scale 50 . foldMap (uncurry drawPiece) . zip palette

drawPiece :: Colour Double -> Piece -> Diagram B
drawPiece c (Piece xs) = foldMap (drawSquare c) xs

drawSquare :: Colour Double -> V2 Int -> Diagram B
drawSquare c o = translate (fromIntegral <$> o)
               $ fc c
               $ square 1

palette :: [Colour Double]
palette = [red, yellow, blue, brown, black, green,
           cyan, salmon, orange, gold, gray, pink]
