{-# Language RecordWildCards #-}
{-|
Module      : Main
Description : Solver for the Loopy game
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Console-based solver for the Loopy game as defined at
https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/loopy.html

This solution enforces the constraint that a solution consists of
active edges in the grid that form a single, continuous, non-intersecting
loop by assigning each edge a direction and sequential ID. The final
ID in the loop is permitted to connect back to the starting element.

-}
module Main where

import           Booleans (MonadSAT)
import           Control.Applicative (liftA2)
import           Control.Monad (replicateM)
import           Data.Char (intToDigit, digitToInt, isDigit)
import           Data.List (transpose)
import           Data.Maybe (catMaybes)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Ersatz
import           Prelude hiding ((&&),(||),all, any, and, or, not)
import           Select
import           SparseMap (SparseMap)
import qualified SparseMap
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           Data.Traversable
import           Data.Foldable (toList, for_)
import           Data.List (tails)

import           Palisade.Regions

data Puzzle = Puzzle Int Int Int (Map Coord Int)
  deriving Show

parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle (read szStr) w h cells
  where
    szStr:ls = lines str
    w = maximum (0 : map length ls)
    h = length ls
    cells = Map.fromList
              [ (C x y,digitToInt c)
                | (y,l) <- zip [0..] ls
                , (x,c) <- zip [0..] l
                , isDigit c ]


cellLocations :: Int -> Int -> [Coord]
cellLocations w h = [ C x y | x <- [0..w-1], y <- [0..h-1] ]

------------------------------------------------------------------------
-- Solver
------------------------------------------------------------------------

-- | Given a puzzle determine an assignment of edges that satisfies
-- the puzzle's clues unless one is impossible.
solvePuzzleIO :: Puzzle -> IO (Maybe [Region])
solvePuzzleIO p =
  do result <- solveWith minisat (solvePuzzle p)
     case result of
       (Satisfied, Just x) -> return (Just (catMaybes x))
       (Unsatisfied, _)    -> return Nothing
       _                   -> fail "solvePuzzleIO: bad result"


-- | Given a puzzle determine an assignment of edges that satisfies
-- the puzzle's clues unless one is impossible.
solvePuzzle :: MonadSAT s m => Puzzle -> m [Select (Maybe Region)]
solvePuzzle (Puzzle r w h clues) =

  do let regions = uniques (concatMap symmetries (sizedRegions r))

     -- generate all possible translations of the regions on the board
     sRegions <- for (cellLocations w h) $ \c ->
                 selectList $ Nothing
                            : [ Just r
                              | r <- translate c <$> regions
                              , inBounds w h r
                              , checkClues clues r ]

     -- check that every cell is contained in exactly one region
     for_ (cellLocations w h) $ \c ->
       let mbInRegion = maybe false (bool . inRegion c)
       in assert (exactlyOne (runSelectWith mbInRegion <$> sRegions))

     return sRegions


checkClues :: Map Coord Int -> Region -> Bool
checkClues clues r = all aux (regionCoords r)
  where
    aux c =
      case Map.lookup c clues of
        Nothing -> true
        Just n  -> n == length [() | c' <- cardinalNeighbors c, not (inRegion c' r)]


inBounds :: Int -> Int -> Region -> Bool
inBounds w h r = x2 < w && y2 < h
  where
    C x2 y2 = maxCoord r


-- | Returns 'true' when at most one element in the given list-like structure
-- is 'true'.
exactlyOne :: (Foldable t, Boolean a) => t a -> a
exactlyOne xs = one && not two
  where
    (one,two) = foldl aux (false,false) xs
    aux (one,two) x = (one || x, two || one && x)

------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------

main :: IO ()
main =
  do puzzle <- parsePuzzle <$> getFile
     result <- solvePuzzleIO puzzle
     case result of
       Just solution ->
         do putStr (renderSolution puzzle solution)
       Nothing ->
         do putStrLn "No solution possible!"
            return ()

-- | Returns either the file specified in the command arguments, stdin
-- contents if @-@ is specified, or fails with an error message.
getFile :: IO String
getFile =
  do args <- getArgs
     case args of
       ["-"] -> do getContents
       [fn ] -> do readFile fn
       _     -> do hPutStrLn stderr "Usage: Palisade FILENAME"
                   exitFailure


------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

data Orient = Horiz | Vert

renderSolution :: Puzzle -> [Region] -> String
renderSolution (Puzzle _ w h clues) rs =
  rearrange
  [ [ [[drawCorner eU eL eR eD, if eR then '─' else ' ']
      ,[if eD then '│' else ' ',
        drawClue x y (Map.lookup (C x y) clues)]]
     | x <- [0..w]
     , let eR = edge       (C x y)  Horiz
     , let eD = edge       (C x y)  Vert
     , let eL = edge (left (C x y)) Horiz
     , let eU = edge (up   (C x y)) Vert
        ]
     | y <- [0..h] ]
  where
    regionIds :: Map Coord Int
    regionIds = Map.fromList [ (c,i) | (i,r) <- zip [0..] rs, c <- regionCoords r ]

    edge :: Coord -> Orient -> Bool
    edge c Horiz = Map.lookup c regionIds
                /= Map.lookup (up c) regionIds
    edge c Vert  = Map.lookup c regionIds
                /= Map.lookup (left c) regionIds

    -- rows of cells of (lines in cell) to single string
    rearrange :: [[[String]]] -> String
    rearrange = unlines . concatMap (map concat . transpose)

    drawClue :: Int -> Int -> Maybe Int -> Char
    drawClue _ _ (Just n) = intToDigit n
    drawClue x y Nothing
      | x < w && y < h = '·'
      | otherwise      = ' ' -- right and bottom edge

    drawCorner u l r d
      | u, d, l, r = '┼'

      | u, d, l    = '┤'
      | u, d,    r = '├'
      | u,    l, r = '┴'
      |    d, l, r = '┬'

      | u, d       = '│'
      | u,    l    = '┘'
      | u,       r = '└'
      |    d, l    = '┐'
      |    d,    r = '┌'
      |       l, r = '─'

      | u          = '╷'
      |    d       = '╵'
      |       l    = '╴'
      |          r = '╶'

      | otherwise = ' '
