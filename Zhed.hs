module Main where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldl', traverse_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Traversable (for)
import Prelude hiding ((&&), (||), not)
import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr)

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
    upd (Coord xMax yMax) (Coord x y) = Coord (max xMax y) (max yMax y)


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
  Int    {- ^ solution length   -} ->
  Puzzle {- ^ puzzle parameters -} ->
  IO (Maybe [(Coord,Int,Dir)])
solveForMoves n p =
  do hPutStr stderr ("Attempting solution with " ++ show n ++ " moves: ")
     startTime <- getCurrentTime
     result    <- solveWith minisat (existsSolution n p)
     endTime   <- getCurrentTime
     hPutStrLn stderr (show (endTime `diffUTCTime` startTime))
     case result of
       (Satisfied, Just solution) ->
         return (Just [(a,b,c) | ((a,b),c) <- solution])
       (Unsatisfied, _) -> return Nothing
       other -> fail ("Unexpected solver result: " ++ show other)


-- | Solve the given puzzle in as few moves as possible.
solve :: Puzzle -> IO (Maybe [(Coord, Int, Dir)])
solve puzzle =
  do let n = length (puzzleSquares puzzle)
     traverse (improve n) =<< solveForMoves n puzzle
  where
    improve n prev =
      do let n' = n-1
         mb <- solveForMoves n' puzzle
         case mb of
           Nothing -> return prev
           Just better -> improve n' better

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
     solveAndPrint (parsePuzzle str)


-- | Solve the given puzzle and print out the solution steps.
solveAndPrint :: Puzzle -> IO ()
solveAndPrint p =
  do Just result <- solve p
     mapM_ print result


parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle target squares
  where
    target = head [ c | (c, 'X') <- xs ]
    squares = [ (c, digitToInt x) | (c,x) <- xs, isDigit x ]

    xs = [ (Coord x y, cel)
             | (y,row) <- zip [0..] (lines str)
             , (x,cel) <- zip [0..] row
             ]
