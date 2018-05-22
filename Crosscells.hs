module Main where

import System.Environment
import System.Exit
import qualified Data.Map as Map

import Crosscells.Tokens
import Crosscells.Puzzle
import Crosscells.Solver

main :: IO ()
main =
  do name <- getArg
     file <- readFile name
     xs <- solvePuzzle (compile (parseTokens file))
     mapM_ print (Map.toList xs)

getArg :: IO String
getArg =
  do args <- getArgs
     case args of
       [x] -> do return x
       _   -> do putStrLn "Usage: Crosscells PUZZLE_FILENAME"
                 exitFailure
