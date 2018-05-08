{-|
Module      : Main
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
module Main where

import Control.Monad (when)
import Data.Char (digitToInt, isDigit, intToDigit)
import Data.Foldable (foldl', traverse_)
import Data.Traversable (for)
import Prelude hiding ((&&), (||), not, any)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Data.Time

import Ersatz
import Select
import SparseMap (SparseMap)
import qualified SparseMap
import Booleans

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
     moves  <- selectPermutationN n (puzzleSquares puzzle)

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
applyMove' (Coord xmax ymax) board (Coord x y) i dir =
  snd (foldl changeCell (fromIntegral i, board) positions)
  where
    positions =
      case dir of
        U -> [ Coord x z | z <- [y-1, y-2 .. 0] ]
        D -> [ Coord x z | z <- [y+1   .. ymax] ]
        L -> [ Coord z y | z <- [x-1, x-2 .. 0] ]
        R -> [ Coord z y | z <- [x+1   .. xmax] ]


-- | Update the cell at the given coordinate if there are
-- any cells remaining to be placed. Updated count will be
-- decremented in a cell was placed.
changeCell ::
  (Bits, Board) {- ^ initial count and initial board -} ->
  Coord         {- ^ focused coordinate              -} ->
  (Bits, Board) {- ^ updated count and updated board -}
changeCell (remaining, board) coord = (remaining', board')
  where
    cell  = SparseMap.index coord board
    cell' = cell || remaining /== 0

    remaining' = chooseBits (remaining - 1) remaining cell
    board'     = SparseMap.insert coord cell' board

------------------------------------------------------------------------
-- Solver harness
------------------------------------------------------------------------

-- | Solve the given puzzle in a specific number of moves if possible.
solveForMoves ::
  Int     {- ^ solution length          -} ->
  Puzzle  {- ^ puzzle parameters        -} ->
  IO Bool {- ^ True when solution found -}
solveForMoves n p =

  do putStr ("Attempting solution with " ++ show n ++ " moves: ")
     hFlush stdout

     startTime <- getCurrentTime
     result    <- solveWith minisat (existsSolution n p)
     endTime   <- getCurrentTime

     -- print elapsed time
     putStrLn (show (endTime `diffUTCTime` startTime))

     case result of

       (Satisfied, Just solution) ->
         do putStr (renderSolution p solution)
            return True

       (Unsatisfied, _) -> return False

       -- probably would indicate a bug in ersatz
       other -> fail ("Unexpected solver result: " ++ show other)


-- | Solve the given puzzle in as few moves as possible.
solve :: Puzzle -> IO ()
solve puzzle = loop (length (puzzleSquares puzzle))
  where
    loop n =
      do possible <- solveForMoves n puzzle
         when possible (loop (n-1))

------------------------------------------------------------------------
-- Driver logic
------------------------------------------------------------------------

-- | Entry point of the program. Treats command-line arguments as file
-- names of puzzles that need to be solved.
main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> putStrLn "No puzzles"
       xs -> traverse_ fileDriver xs


-- | Read the puzzle file stored at the given path, parse it, and solve it.
fileDriver ::
  FilePath {- ^ path to puzzle file                     -} ->
  IO ()    {- ^ read, parse, solve and print the puzzle -}
fileDriver path =
  do str <- readFile path
     solve (parsePuzzle str)

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

------------------------------------------------------------------------
-- Output format
------------------------------------------------------------------------

-- | Produce a rendering of the solution to a puzzle showing which order
-- to click each square and in which direction.
renderSolution :: Puzzle -> [(Coord, Int, Dir)] -> String
renderSolution puzzle solution =
  unlines [ [ SparseMap.index (Coord x y) txt | x <- [0 .. 3*xMax+2]]
          | y <- [0..yMax] ]
  where
    Coord xMax yMax = puzzleBounds puzzle

    dirChar d = case d of U -> '^'; D -> 'v'; L -> '<'; R -> '>'

    expand (Coord x y, s) = [ (Coord (3*x+i) y, c) | (i,c) <- zip [0..] s ]

    txt = SparseMap.fromList '.'
        $ concatMap expand
        $ [ (Coord x y, "[+]") | Coord x y     <- puzzleTarget  puzzle ]
       ++ [ (Coord x y, "[ ]") | (Coord x y,_) <- puzzleSquares puzzle ]
       ++ [ (Coord x y, [intToDigit d1, intToDigit d2, dirChar d])
             | (n, (Coord x y, _, d)) <- zip [1..] solution
             , let (d1,d2) = quotRem n 10 ]
