{-# Language TypeFamilies #-}
{-# Language PackageImports #-}
module Main where

import "base"         Data.List ((\\), sort, findIndex, nub, mapAccumL)
import "base"         Data.Maybe (fromJust)
import "base"         Control.Applicative (liftA2)
import "base"         Control.Monad (replicateM)
import                Prelude hiding (not, or, and, all, (&&), (||))

import "colour"       Data.Colour.Names
import "diagrams-lib" Diagrams.Prelude (Diagram, Colour, scale, translate, fc, square)
import "diagrams-svg" Diagrams.Backend.SVG.CmdLine (B, mainWith)
import "ersatz"       Ersatz
import "linear"       Linear (V2(V2))
import "mtl"          Control.Monad.State (MonadState)
import "split"        Data.List.Split (splitWhen)

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
  [ p2
  | p1 <- orientations p
  , dx <- [0..7]
  , dy <- [0..7]
  , let p2 = translatePiece (V2 dx dy) p1
  , fits p2
  ]

-- | Predicate to test if all piece coördinates are valid board locations
fits :: Piece -> Bool
fits (Piece xs) = all (`elem` boardPositions) xs

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
-- Piece to board operations

-- | Given a bit indicating if this piece is active construct a board
-- representing the locations covered by this piece.
pieceToBits :: Bit -> Piece -> Board
pieceToBits active (Piece xs) = Board
  [ active && bool (x `elem` xs) | x <- boardPositions ]

-- | Construct a board representing the locations covered by the selected
-- piece in a set of possible choices.
selectPieceMask :: Select Piece -> Board
selectPieceMask (Select xs n) =
  or [ pieceToBits (fromIntegral i === n) o | (i,o) <- zip [0..] xs ]

------------------------------------------------------------------------

-- | A board is a collection of bits indicating whether each location is
-- covered by a piece or not.
newtype Board = Board [Bit] -- Invariant: One element per initialBoard location

-- | Set of valid board locations: 8x8 grid with center 2x2 square removed
boardPositions :: [V2 Int]
boardPositions = liftA2 V2 [0..7] [0..7]
              \\ liftA2 V2 [3..4] [3..4]


-- | Boards are equal when all corresponding locations are equally covered.
instance Equatable Board where
  Board xs === Board ys = xs === ys

-- | Boolean operations on boards are treated as point-wise boolean operations
-- on the locations of boards.
instance Boolean Board where
  bool     = boardOp0 . bool
  (&&)     = boardOp2 (&&)
  (||)     = boardOp2 (||)
  not      = boardOp1 not
  all f    = foldr (\y ys -> f y && ys) true
  any f    = foldr (\y ys -> f y || ys) false
  xor      = boardOp2 xor

-- Lifts a nullary operation on bits to boards
boardOp0 :: Bit -> Board
boardOp0 x = Board (x <$ boardPositions)

-- Lifts a unary operation on bits to boards
boardOp1 :: (Bit -> Bit) -> Board -> Board
boardOp1 f (Board xs) = Board (map f xs)

-- Lifts a binary operation on bits to boards
boardOp2 :: (Bit -> Bit -> Bit) -> Board -> Board -> Board
boardOp2 f (Board xs) (Board ys) = Board (zipWith f xs ys)


------------------------------------------------------------------------
-- Main program logic

main :: IO ()
main =
  do pieces <- loadPieces
     (Satisfied, Just sol) <- solveWith minisat (problem pieces)
     mainWith (drawSolution sol)

-- | Select an arrangement of the pieces that satisfies the covering
-- predicate for this puzzle.
problem :: (HasSAT s, MonadState s m) => [Piece] -> m [Select Piece]
problem pieces =
  do choices <- traverse (select . placements) pieces
     assert (choicePredicate choices)
     return choices

-- | A choice is valid when every location on the board is covered exactly once
choicePredicate :: [Select Piece] -> Bit
choicePredicate choices = true === exactlyOne (selectPieceMask <$> choices)

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

------------------------------------------------------------------------
-- Symbolic variables with dynamic range

-- | Bits needed to distinguish the given number of elements
bitsNeeded :: Int -> Int
bitsNeeded x = fromJust (findIndex (>= x) (iterate (*2) 1))

-- | Generate a variable-bit symbolic term capable of representing the
-- natural numbers below the given bound.
existsNat :: (HasSAT s, MonadState s m) => Int -> m Bits
existsNat bound =
  do x <- Bits <$> replicateM (bitsNeeded bound) exists
     assert (x <? fromIntegral bound)
     return x

------------------------------------------------------------------------
-- Type representing a symbolic choice from a set

-- | A set of choices and an index of the chosen element of that set
data Select a = Select [a] Bits

select :: (HasSAT s, MonadState s m) => [a] -> m (Select a)
select xs =
  do n <- existsNat (length xs)
     return (Select xs n)

instance Codec (Select a) where
  type Decoded (Select a) = a
  encode x = Select [x] 0
  decode sol (Select xs i) =
    do j <- decode sol i
       return (xs !! fromIntegral j)
