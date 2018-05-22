module Main where

import System.Environment
import qualified Data.Map as Map

import Crosscells.Tokens
import Crosscells.Puzzle
import Crosscells.Solver

main :: IO ()
main =
  do [name] <- getArgs
     file <- readFile name
     xs <- solvePuzzle (compile (parseTokens file))
     mapM_ print (Map.toList xs)

