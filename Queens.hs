module Main where

import Ersatz
import Booleans
import Control.Monad
import Data.List (transpose, mapAccumL)
import System.Environment
import Prelude hiding (not, (||), (&&), all)

import BoxDrawing
import Coord

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
  do n   <- getSize
     res <- getModel (problem n)
     case res of
       Just ys -> putStr (render n ys)
       Nothing -> putStrLn "Impossible"

render :: Int -> [[Bool]] -> String
render n xs = renderGrid n n drawEdge drawCell
  where
    drawEdge (C x y) o
      | 0 == x && o == Vert  ||
        0 == y && o == Horiz ||
        n == x || n == y     = Just Double
      | otherwise            = Just Thin
    drawCell (C x y)
      | xs !! y !! x     = '♕'
      | even x == even y = '·'
      | otherwise        = ' '
