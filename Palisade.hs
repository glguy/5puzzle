{-|
Module      : Main
Description : Solver for the Palisade game
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Console-based solver for the Palisade game as defined at
https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/palisade.html


-}
module Main where

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
import           Booleans (MonadSAT, getModel)
import           BoxDrawing
import           Coord

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
solvePuzzleIO = fmap (fmap catMaybes) . getModel . solvePuzzle


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
                              , isSolid r
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

renderSolution :: Puzzle -> [Region] -> String
renderSolution (Puzzle _ w h clues) rs = renderGrid w h drawEdge drawCell
  where
    regionIds :: Map Coord Int
    regionIds = Map.fromList [ (c,i) | (i,r) <- zip [0..] rs, c <- regionCoords r ]

    needsEdge c Horiz = Map.lookup c regionIds /= Map.lookup (up   c) regionIds
    needsEdge c Vert  = Map.lookup c regionIds /= Map.lookup (left c) regionIds

    drawEdge c o
      | needsEdge c o = Just Double
      | otherwise     = Nothing

    drawCell c@(C x y) = maybe "·" show (Map.lookup c clues)
