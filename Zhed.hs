{-|
Module      : Main
Description : Solver for the Zhed game
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Console-based driver for "Zhed.Puzzle"

-}
module Main where

import Control.Monad (when)
import Data.Char (intToDigit)
import Data.Foldable (foldl', traverse_)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Console.GetOpt
import System.Exit

import Data.Time

import Zhed.Picture
import Zhed.Puzzle

import qualified SparseMap

------------------------------------------------------------------------
-- Solver harness
------------------------------------------------------------------------

-- | Solve the given puzzle in a specific number of moves if possible.
solveForMoves ::
  Int            {- ^ solution length                              -} ->
  Puzzle         {- ^ puzzle parameters                            -} ->
  Maybe FilePath {- ^ optional path to write SVG rendered solution -} ->
  IO Bool        {- ^ True when solution found                     -}
solveForMoves n p svgPath =

  do putStr ("Attempting solution with " ++ show n ++ " moves: ")
     hFlush stdout

     startTime <- getCurrentTime
     result    <- solvePuzzle n p
     endTime   <- getCurrentTime

     -- print elapsed time
     putStrLn (show (endTime `diffUTCTime` startTime))

     case result of

       Just solution ->
         do putStr (renderSolution p solution)
            traverse_ (renderSolutionSVG p solution) svgPath
            return True

       Nothing -> return False


-- | Solve the given puzzle in as few moves as possible.
solve :: Options -> Puzzle -> IO ()
solve opts puzzle = loop initial
  where
    initial = case optMoves opts of
                Just n  -> n
                Nothing -> length (puzzleSquares puzzle)
    loop n =
      do possible <- solveForMoves n puzzle (optSvgOutput opts)
         when (possible && optMinimize opts) (loop (n-1))

------------------------------------------------------------------------
-- Driver logic
------------------------------------------------------------------------

-- | Entry point of the program. Treats command-line arguments as file
-- names of puzzles that need to be solved.
main :: IO ()
main =
  do (opts, files) <- getOptions
     traverse_ (fileDriver opts) files


-- | Read the puzzle file stored at the given path, parse it, and solve it.
fileDriver ::
  Options  {- ^ configuration options                   -} ->
  FilePath {- ^ path to puzzle file                     -} ->
  IO ()    {- ^ read, parse, solve and print the puzzle -}
fileDriver opts path =
  do putStrLn ("Processing: " ++ path)
     str <- readFile path
     solve opts (parsePuzzle str)

------------------------------------------------------------------------
-- Command-line options parsing
------------------------------------------------------------------------

data Options = Options
  { optMoves :: Maybe Int
  , optMinimize :: Bool
  , optSvgOutput :: Maybe FilePath }

-- | Load the command arguments and process the flags returning the file
-- name parameters and parsed options.
getOptions :: IO (Options, [String])
getOptions =
  do args <- getArgs
     case getOpt RequireOrder options args of
       (fs, xs, []) -> return (opts, xs)
         where opts = foldl' (flip id) defaultOptions fs
       (_, _, errs) ->
         do traverse_ putStr (usageInfo "Zhed [FLAGS] FILENAMES..." options : errs)
            exitFailure

-- | Default solver options.
defaultOptions :: Options
defaultOptions = Options
  { optMoves    = Nothing
  , optMinimize = True
  , optSvgOutput = Nothing }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['n'] []
      (ReqArg (\str o -> o { optMoves = Just (read str) }) "NUMBER")
      "Search using a specific number of moves"
  , Option ['x'] []
      (NoArg (\o -> o { optMinimize = False }))
      "Disable automatic minimization search"
  , Option ['s'] []
      (ReqArg (\path o -> o { optSvgOutput = Just path }) "PATH")
      "Output path for SVG rendered solutions"
  ]

------------------------------------------------------------------------
-- Output format
------------------------------------------------------------------------

-- | Produce a rendering of the solution to a puzzle showing which order
-- to click each square and in which direction.
renderSolution :: Puzzle -> [(Coord, Int, Dir)] -> String
renderSolution puzzle solution =
  unlines [ intercalate " "
            [ SparseMap.index (Coord x y) txt | x <- [0 .. xMax]]
          | y <- [0..yMax] ]
  where
    Coord xMax yMax = puzzleBounds puzzle

    dirChar d = case d of U -> '^'; D -> 'v'; L -> '<'; R -> '>'

    txt = SparseMap.fromList " . "
        $ [ (Coord x y, "[+]") | Coord x y     <- puzzleTarget  puzzle ]
       ++ [ (Coord x y, "[ ]") | (Coord x y,_) <- puzzleSquares puzzle ]
       ++ [ (Coord x y, [intToDigit d1, intToDigit d2, dirChar d])
             | (n, (Coord x y, _, d)) <- zip [1..] solution
             , let (d1,d2) = quotRem n 10 ]
