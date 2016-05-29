module Main where

import Ersatz
import Booleans
import Control.Monad
import Data.List (transpose, mapAccumL)
import System.Environment
import Prelude hiding (not, (||), (&&), all)

problem :: MonadSAT s m => Int -> m [[Bit]]
problem n =
  do ys <- replicateM n
         $ replicateM n exists
     assert (all (coverOne  true ) ys)
     assert (all (coverOne  true ) (transpose ys))
     assert (all zeroOrOne (diagonals ys))
     assert (all zeroOrOne (diagonals (reverse ys)))
     return ys

zeroOrOne :: [Bit] -> Bit
zeroOrOne xs = nor twoOrMore
  where
  (_oneOrMore, twoOrMore) = mapAccumL aux false xs
  aux done x = (x || done, x && done)

diagonals :: [[a]] -> [[a]]
diagonals xs = transpose (zipWith drop [0..] xs)
            ++ transpose (zipWith drop [1..] (transpose xs))


getSize :: IO Int
getSize =
  do args <- getArgs
     case args of
       [] -> return 8
       str:_ -> readIO str

main :: IO ()
main =
  do n <- getSize
     res <- solveWith minisat (problem n)
     case res of
       (Satisfied, Just ys) -> putStr (render ys)
       (Unsatisfied, _    ) -> putStrLn "Impossible"
       _ -> putStrLn "Failure"

render :: [[Bool]] -> String
render = unlines . map (map render1)
  where
  render1 True = '*'
  render1 False = '.'
