module Main where

import Booleans
import Data.Char
import Data.Word (Word8)
import Ersatz
import System.Environment
import Data.Monoid ((<>))
import Prelude hiding (not, and, or, any, all, (&&), (||))
import           Data.Map (Map)
import qualified Data.Map as Map

-- | Coordinates on sudoku boards from (0,0) to (8,8)
data Coord = Coord { row, col :: !Int } deriving (Eq, Ord, Show)

-- | Sudoku board
type Board = Map Coord Word8

-- | Symbolic sudoku board
type SBoard = Map Coord Bit4

------------------------------------------------------------------------
-- Definition of a Sudoku board
------------------------------------------------------------------------

-- | Grids filled with numbers from 1 to 9 where each row, column and
-- box contain unique elements.
boardExists :: MonadSAT s m => m SBoard
boardExists =
  do board <- sequence
            $ Map.fromList [ (Coord r c, exists) | r <- [0..8]
                                                 , c <- [0..8]]
     assert (all inRange board)
     assert (all unique (generateGroups board))
     return board

-- | Predicate for numbers from 1 to 9, inclusive.
inRange :: Bit4 -> Bit
inRange x = encode 1 <=? x && x <=? encode 9

-- | Generate the various row, column, and box groups for a board
generateGroups :: Map Coord a -> [[a]]
generateGroups = foldMap extractGroup [row,col,box] -- so clever

-- | Compute groups of elements whose keys mapped to the same characteristic
extractGroup :: Ord k' => (k -> k') -> Map k v -> [[v]]
extractGroup char = Map.elems . Map.mapKeysWith (++) char . Map.map (:[])

-- | Map a grid coordinate to a number identifying the 3x3 box it is in
box :: Coord -> Int
box c = row c`quot`3 * 3 + col c`quot`3

------------------------------------------------------------------------

parseConstraints :: String -> Board
parseConstraints str = Map.fromList
  [ (Coord r c, toEnum (digitToInt cell))
  | (r, line) <- zip [0..8] (lines str)
  , (c, cell) <- zip [0..8] line
  , '1' <= cell, cell <= '9'
  ]

matchConstraints :: Board -> SBoard -> Bit
matchConstraints constraints board =
  and (Map.intersectionWith (===) board (encode constraints))

problem :: MonadSAT s m => Board -> m SBoard
problem constraints = boardExists `checking` matchConstraints constraints

getConstraintFileName :: IO FilePath
getConstraintFileName =
  do args <- getArgs
     return $! case args of
                 []  -> "sudoku.txt"
                 x:_ -> x

main :: IO ()
main =
  do fn          <- getConstraintFileName
     constraints <- parseConstraints <$> readFile fn
     res         <- solveWith minisat (problem constraints)
     case res of
       (Satisfied, Just sol) -> putStr (render sol)
       (Unsatisfied, _     ) -> putStrLn "No solution"
       (Unsolved   , _     ) -> putStrLn "Solver gave up?"
       (Satisfied, Nothing ) -> fail "Unable to parse solution"

------------------------------------------------------------------------
-- Solution rendering
------------------------------------------------------------------------

render :: Board -> String
render m = unlines [ concat [ Map.findWithDefault "." (Coord r c) m'
                            | c <- [0..8]]
                   | r <- [0..8] ]
  where
  m' = Map.map show m
