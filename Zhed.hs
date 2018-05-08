module Main where

import Control.Monad (when)
import Data.Char (digitToInt, isDigit, intToDigit)
import Data.Foldable (foldl', traverse_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Traversable (for)
import Prelude hiding ((&&), (||), not)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Data.Time

import Ersatz
import Select
import SparseMap (SparseMap)
import qualified SparseMap
import Booleans

data Coord = Coord !Int !Int
  deriving (Eq, Ord, Show, Read)

data Dir = U | D | L | R
  deriving (Eq, Ord, Read, Show)

data Puzzle = Puzzle
  { puzzleTarget  :: Coord          -- ^ coodinate of goal
  , puzzleSquares :: [(Coord, Int)] -- ^ initial list of squares
  }
  deriving (Read, Show)

type Board = SparseMap Coord Bit


-- | Compute a board where initial squares are activated and others are not.
initialBoard :: Puzzle -> Board
initialBoard = SparseMap.trueList . map fst . puzzleSquares


-- | Compute the coordinate on the rectangular board furthest from the origin.
puzzleBounds :: Puzzle -> Coord
puzzleBounds (Puzzle target squares) = foldl' upd target (map fst squares)
  where
    upd (Coord xMax yMax) (Coord x y) = Coord (max xMax x) (max yMax y)


-- | Given a puzzle compute a sequence of moves that will cause the target
-- cell to be activated.
existsSolution ::
  MonadSAT s m =>
  Int                                   {- ^ solution length       -} ->
  Puzzle                                {- ^ puzzle parameters     -} ->
  m [(Select (Coord, Int), Select Dir)] {- ^ ordered list of moves -}
existsSolution n puzzle =

  do let b0     = initialBoard puzzle
         bounds = puzzleBounds puzzle

     -- chose the order cells will be clicked
     moves  <- selectPermutationN n (puzzleSquares puzzle)

     -- chose a direction for each click
     moves' <- for moves $ \move ->
                 do dir <- select (U :| [D,L,R])
                    return (move, dir)

     -- update the initial board given the chosen moves
     let bN = foldl' (\b (s,d) -> applyMove bounds b s d) b0 moves'

     -- ensure that the target cell on the board is reached
     assert (SparseMap.index (puzzleTarget puzzle) bN)

     return moves'


-- | Update a board given a particular move choice. This function
-- uses 'applyMove'' for the placement logic, and flattens the
-- symbolic choices into the logical values of the cells of the
-- board.
applyMove ::
  Coord               {- ^ board bounds          -} ->
  Board               {- ^ current board state   -} ->
  Select (Coord, Int) {- ^ move start and length -} ->
  Select Dir          {- ^ move direction        -} ->
  Board               {- ^ updated board state   -}
applyMove bounds b square dir =
  runSelect $
    do (coord, n) <- square
       d          <- dir
       return (applyMove' bounds b coord n d)


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
applyMove' (Coord xmax ymax) board (Coord x y) n dir =
  snd (foldl changeCell (fromIntegral n, board) positions)
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

     putStrLn (show (endTime `diffUTCTime` startTime))

     case result of
       (Satisfied, Just solution) ->
         do let solution' = [(a,b,c) | ((a,b),c) <- solution]
            putStr (renderSolution p solution')
            return True
       (Unsatisfied, _) -> return False
       other -> fail ("Unexpected solver result: " ++ show other)


-- | Solve the given puzzle in as few moves as possible.
solve :: Puzzle -> IO ()
solve puzzle = loop (length (puzzleSquares puzzle))
  where
    loop n =
      do possible <- solveForMoves n puzzle
         when possible (loop (n-1))

------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> putStrLn "No puzzles"
       xs -> traverse_ fileDriver xs


-- | Read the puzzle file stored at the given path, parse it, and solve it.
fileDriver ::
  FilePath {- ^ path to puzzle file -} ->
  IO ()
fileDriver path =
  do str <- readFile path
     solve (parsePuzzle str)


parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle target squares
  where
    target = head [ c | (c, 'X') <- xs ]
    squares = [ (c, digitToInt x) | (c,x) <- xs, isDigit x ]

    xs = [ (Coord x y, cel)
             | (y,row) <- zip [0..] (lines str)
             , (x,cel) <- zip [0..] row
             ]

------------------------------------------------------------------------

-- | Produce a rendering of the solution to a puzzle showing which order
-- to click each square and in which direction.
renderSolution :: Puzzle -> [(Coord, Int, Dir)] -> String
renderSolution puzzle solution =
  unlines [ [ SparseMap.index (Coord x y) txt
                | x <- [0 .. 4*xMax+2]]
              | y <- [0..yMax] ]
  where
    Coord xMax yMax = puzzleBounds puzzle

    dirChar d = case d of U -> '^'; D -> 'v'; L -> '<'; R -> '>'

    expand (Coord x y, s) = [ (Coord (4*x+i) y, c) | (i,c) <- zip [0..] s ]

    txt = SparseMap.fromList '.'
        $ concatMap expand
        $ [ (Coord x y, "[+]") | Coord x y <- [puzzleTarget puzzle] ]
       ++ [ (Coord x y, "[ ]") | (Coord x y,_) <- puzzleSquares puzzle ]
       ++ [ (Coord x y, [intToDigit (n`div`10), intToDigit (n`mod`10), dirChar d])
             | (n, (Coord x y, _, d)) <- zip [1..] solution ]
