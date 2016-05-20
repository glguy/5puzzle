{-# Language TypeFamilies #-}
module Main where

import Data.List (sort, findIndex, nub, mapAccumL)
import Data.List.Split (splitWhen)
import Data.Maybe
import Linear
import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import Data.Set ( Set )
import Ersatz
import Control.Monad.State (MonadState)
import Diagrams.Prelude hiding ((===))
import Diagrams.Backend.SVG.CmdLine

import Prelude hiding (not, or, and, all, (&&), (||))

newtype Piece = Piece [V2 Int]
  deriving (Show, Eq)

loadPieces :: IO [Piece]
loadPieces =
  do txt <- readFile "pieces.txt"
     let rawPieces = splitWhen null (lines txt)
     return (map parsePiece rawPieces)

parsePiece :: [String] -> Piece
parsePiece xs = Piece [ V2 rowIx colIx
                        | (rowIx, row) <- addIx xs
                        , (colIx, '*') <- addIx row
                        ]

------------------------------------------------------------------------

translatePiece :: V2 Int -> Piece -> Piece
translatePiece offset (Piece xs) = Piece (liftA2 (+) offset <$> xs)

-- | Translate a piece such that it's minimum row and column
-- are at index 0.
retranslate :: Piece -> Piece
retranslate (Piece []) = Piece []
retranslate (Piece xs) = translatePiece (negate offset) (Piece xs)
  where
  offset = foldl1 (liftA2 min) xs

-- | Arrange the coördinates of a piece in a sorted order to
-- make the pieces suitable for comparison with each other.
normalizePiece :: Piece -> Piece
normalizePiece (Piece xs) = Piece (sort xs)

-- | Find all possible positions that a given piece could fit on an
-- empty board.
orientations :: Piece -> [Piece]
orientations (Piece xs)
  = nub
  $ map normalizePiece
  $ filter fits
  [ translatePiece (V2 dx dy) p
  | f <- [id, \(V2 x y) -> V2 y x]
  , g <- [id, over _x negate]
  , h <- [id, over _y negate]
  , let p = retranslate (Piece (h . g . f <$> xs))
  , dx <- [0..7]
  , dy <- [0..7]
  ]

-- | Set of valid board locations
initialBoard :: Set (V2 Int)
initialBoard = Set.fromList (liftA2 V2 [0..7] [0..7])
               Set.\\
               Set.fromList (liftA2 V2 [3..4] [3..4])

-- | Predicate to test if all piece coördinates are valid board locations
fits :: Piece -> Bool
fits (Piece xs) = all (`Set.member` initialBoard) xs

-- | Given a bit indicating if this piece is active construct a board
-- representing the locations covered by this piece.
pieceToBits :: Bit -> Piece -> Board
pieceToBits active (Piece xs) = Board
  [ active && bool (V2 row col `elem` xs) | V2 row col <- Set.toList initialBoard ]

-- | Construct a board representing the locations covered by the selected
-- piece in a set of possible choices.
selectPieceMask :: Select Piece -> Board
selectPieceMask (Select xs n) =
  or [ pieceToBits (fromIntegral i === n) o | (i,o) <- addIx xs ]

------------------------------------------------------------------------

-- | A board is a collection of bits indicating whether each location is
-- covered by a piece or not.
newtype Board = Board [Bit] -- Invariant: One element per initialBoard location

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
boardOp0 x = Board (x <$ Set.toList initialBoard)

-- Lifts a unary operation on bits to boards
boardOp1 :: (Bit -> Bit) -> Board -> Board
boardOp1 f (Board xs) = Board (map f xs)

-- Lifts a binary operation on bits to boards
boardOp2 :: (Bit -> Bit -> Bit) -> Board -> Board -> Board
boardOp2 f (Board xs) (Board ys) = Board (zipWith f xs ys)

------------------------------------------------------------------------



main :: IO ()
main =
  do pieces <- loadPieces
     (Satisfied, Just sol) <- solveWith minisat (problem pieces)
     mainWith (drawSolution sol)

-- | Select an arrangement of the pieces that satisfies the covering
-- predicate for this puzzle.
problem :: (HasSAT s, MonadState s m) => [Piece] -> m [Select Piece]
problem pieces =
  do choices <- traverse (select . orientations) pieces
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

addIx :: [a] -> [(Int,a)]
addIx = zip [0..]

------------------------------------------------------------------------

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
