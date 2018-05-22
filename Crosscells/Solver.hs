{-# Language ConstraintKinds #-}
module Crosscells.Solver (solvePuzzle) where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map ( Map )
import Data.Maybe
import qualified Data.Map as Map

import Ersatz
import Select
import Booleans (MonadSAT)

import Crosscells.Puzzle
import Crosscells.Region


solvePuzzle :: Puzzle -> IO (Map Coord (Maybe Op))
solvePuzzle p =
  do (Satisfied, Just xs) <- solveWith minisat (puzzleToSolution p)
     return xs

puzzleToSolution :: MonadSAT s m => Puzzle -> m (Select (Map Coord (Maybe Op)))
puzzleToSolution cs =
  do boxes <- traverse optional (Map.fromList (concatMap snd cs))
     for_ cs $ \(c,xs) ->
       let xs' = map (boxes Map.!) (map fst xs) in
       case c of
         Arith n -> assertEq n (compute xs')
         Count n -> assertEq n (count   xs')
     return (sequenceA boxes)

optional :: MonadSAT s m => a -> m (Select (Maybe a))
optional op = select (Nothing :| [Just op])

assertEq :: MonadSAT s m => Eq a => a -> Select a -> m ()
assertEq x my = assert (runSelect ( bool . (x==) <$> my ))

selectedElts :: [Select (Maybe a)] -> Select [a]
selectedElts xs = catMaybes <$> sequenceA xs

count :: [Select (Maybe a)] -> Select Int
count xs = length <$> selectedElts xs

compute :: [Select (Maybe Op)] -> Select Int
compute xs = foldl' eval 0 <$> selectedElts xs

eval :: Int -> Op -> Int
eval x (Add y) = x + y
eval x (Mul y) = x * y
