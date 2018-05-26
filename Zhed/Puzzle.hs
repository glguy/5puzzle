{-|
Module      : Zhed.Puzzle
Description : Solver for the Zhed game
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Zhed is a grid-based puzzle game where the player searches for
an activation sequence of the squares of the puzzle that causes
at least one of the target locations to be covered.

Each square in the puzzle has a number and can be activated
at most one time. Activation happens in a cardinal direction.
Upon activation the given number of squares is placed sequentially
in the selected direction. Covered squares are skipped over allowing
an activation to reach further if some of the neighboring squares
are already covered. Activated and unactivated squares are treated
as covered.

In this example a digit represents a square able to be activated,
an @X@ represents the target, and a @*@ represents a covered square.

@
..2..X   ..2..X   ..2..X   ..2*.X   ..****
......   ......   ......   ...*..   ...*..
2.....   2*....   ****..   ****..   ****..
.1.2..   .*.2..   .*.2..   .*.*..   .*.*..
@

-}
module Zhed.Puzzle where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldl')
import Data.Traversable (for)
import Prelude hiding ((&&), (||), not, any)

import Ersatz
import Select
import SparseMap (SparseMap)
import qualified SparseMap
import ChooseBit
import Booleans (MonadSAT)

-- | Two-dimensional grid coordinate
data Coord = Coord !Int !Int
  deriving (Eq, Ord, Show, Read)

-- | Cardinal directions: up, down, left, right
data Dir = U | D | L | R
  deriving (Eq, Ord, Read, Show)

-- | Parameters for a particular Zhed puzzle
data Puzzle = Puzzle
  { puzzleTarget  :: [Coord]        -- ^ coodinates of goal
  , puzzleSquares :: [(Coord, Int)] -- ^ initial list of squares
  }
  deriving (Read, Show)

-- | Representation of the current state of a board. Coordinates
-- are mapped to a true bit when the cell is filled and false otherwise.
type Board = SparseMap Coord Bit


-- | Compute a board where initial squares are activated and others are not.
initialBoard :: Puzzle -> Board
initialBoard = SparseMap.trueList . map fst . puzzleSquares


-- | Compute the coordinate on the rectangular board furthest from the origin.
puzzleBounds :: Puzzle -> Coord
puzzleBounds (Puzzle target squares) = foldl' upd (Coord 0 0) (target ++ map fst squares)
  where
    upd (Coord xMax yMax) (Coord x y) = Coord (max xMax x) (max yMax y)


-- | Given a puzzle compute a sequence of moves that will cause the target
-- cell to be activated.
existsSolution ::
  MonadSAT s m =>
  Int                            {- ^ solution length       -} ->
  Puzzle                         {- ^ puzzle parameters     -} ->
  m [(Select (Coord, Int, Dir))] {- ^ ordered list of moves -}
existsSolution n puzzle =

  do -- chose the order cells will be clicked
     -- reverse the order because its cheaper to fix earlier
     -- elements in the list than later ones.
     moves <- reverse <$> selectPermutationN n (puzzleSquares puzzle)

     let combine x y = do (a,b) <- x; c <- y; return (a,b,c)

     -- chose a direction for each click
     moves' <- for moves $ \move ->
                 combine move <$> selectList [U,D,L,R]

     -- update the initial board given the chosen moves
     let bounds = puzzleBounds puzzle
         board  = foldl' (applyMove bounds)
                         (initialBoard puzzle)
                         moves'

     -- ensure that the target cell on the board is reached
     assert (any (`SparseMap.index` board) (puzzleTarget puzzle))

     return moves'


-- | Update a board given a particular move choice. This function
-- uses 'applyMove'' for the placement logic, and flattens the
-- symbolic choices into the logical values of the cells of the
-- board.
applyMove ::
  Coord                    {- ^ board bounds                  -} ->
  Board                    {- ^ current board state           -} ->
  Select (Coord, Int, Dir) {- ^ move start, length, direction -} ->
  Board                    {- ^ updated board state           -}
applyMove bounds board =
  runSelectWith (\(c, i, d) -> applyMove' bounds board c i d)


-- | This function updates a board by placing the
-- given number of squares in a given direction starting
-- at the focused coordinate.
applyMove' ::
  Coord {- ^ board size          -} ->
  Board {- ^ current board state -} ->
  Coord {- ^ starting coordinate -} ->
  Int   {- ^ squares to place    -} ->
  Dir   {- ^ placement direction -} ->
  Board {- ^ updated board       -}
applyMove' bounds board start i dir = out
  where
    (_,_,out) = foldl changeCell
                      (i, fromIntegral i, board)
                      (coordsForDir bounds start dir)


-- | Compute the list of coordinates moving in the given direction
-- from a starting point until the boundary of the board is reached.
-- The starting point is not included in the list.
coordsForDir ::
  Coord {- ^ bounds    -} ->
  Coord {- ^ start     -} ->
  Dir   {- ^ direction -} ->
  [Coord]
coordsForDir (Coord xmax ymax) (Coord x y) dir =
  case dir of
    U -> [ Coord x z | z <- [y-1, y-2 .. 0] ]
    D -> [ Coord x z | z <- [y+1   .. ymax] ]
    L -> [ Coord z y | z <- [x-1, x-2 .. 0] ]
    R -> [ Coord z y | z <- [x+1   .. xmax] ]


-- | Update the cell at the given coordinate if there are
-- any cells remaining to be placed. Updated count will be
-- decremented in a cell was placed.
changeCell ::
  (Int, Bits, Board) {- ^ initial count and initial board -} ->
  Coord              {- ^ focused coordinate              -} ->
  (Int, Bits, Board) {- ^ updated count and updated board -}
changeCell (actual, remaining, board) coord = (actual-1, remaining', board')
  where
    cell  = SparseMap.index coord board
    cell' = bool (actual > 0) || cell || remaining /== 0

    remaining' = chooseBit (remaining - 1) remaining cell
    board'     = SparseMap.insert coord cell' board

------------------------------------------------------------------------
-- Solver harness
------------------------------------------------------------------------

-- | Solve the given puzzle in a specific number of moves if possible.
solvePuzzle ::
  Int     {- ^ solution length          -} ->
  Puzzle  {- ^ puzzle parameters        -} ->
  IO (Maybe [(Coord, Int, Dir)]) {- ^ solution if found -}
solvePuzzle n p =

  do result    <- solveWith minisat (existsSolution n p)

     case result of
       (Satisfied, solution) -> return solution
       (Unsatisfied, _) -> return Nothing
       -- probably would indicate a bug in ersatz
       other -> fail ("Unexpected solver result: " ++ show other)

------------------------------------------------------------------------
-- Input format
------------------------------------------------------------------------

-- | Parse a puzzle string.
parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle target squares
  where
    target  = [ c | (c, 'X') <- xs ]
    squares = [ (c, digitToInt x) | (c,x) <- xs, isDigit x ]

    xs = [ (Coord x y, cel)
             | (y,row) <- zip [0..] (lines str)
             , (x,cel) <- zip [0..] row
             ]
