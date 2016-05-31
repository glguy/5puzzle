{-# Language TypeFamilies #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Main where

import RegExp.AST
import RegExp.Match     (match)

import Ersatz
import Booleans

import Control.Monad
import Data.List (intercalate,transpose)
import Prelude hiding ((&&),(||),all,and,any,or,not)

main :: IO ()
main =
  do (Satisfied, Just xs) <- solveWith minisat (nonoSolve nonoPuzzle2)
     let asChar True  = '*'
         asChar False = ' '
     putStr $ unlines $ map (map asChar) xs

data NonoPuzzle = NonoPuzzle
  { nonorows, nonocols :: [[Int]]
  , nonoextra :: [(Int,Int)]
  }

nonoPuzzle :: NonoPuzzle
nonoPuzzle = NonoPuzzle
  [[2],[1,2],[1,1], [2],[1],[3],[3],[2,2],[2,1],[2,2,1],[2,3],[2,2]]
  [[2,1],[1,3],[2,4],[3,4],[4],[3],[3],[3],[2],[2]]
  []

nonoPuzzle2 :: NonoPuzzle
nonoPuzzle2 = NonoPuzzle
  [[7],[10],[1,4,1],[1,1,2,1,1],[1,1,2,1,1],
  [1,1,1,2],[1,1,1,1,1,2],[1,1,5,2],[1,1,1,3],[1,1,3,4],
  [2,2,6,2],[1,2,2,6,3],[2,3,4,2,2],[2,2,5,2,2],[1,7,2,2],
  [2,5,3,2],[4,5,2],[12,2],[10,2],[7,2],
  [1,2,3],[15],[15],[4,10],[4,9],
  [3,9,1],[2,9,3],[1,7,1,1,3],[1,5,2,1,3],[1,4,2,1,1,1,2]]
  [[2,5],[1,2,3],[6,3,3,3],[1,1,1,2,4],[5,1,2,3,4],
  [1,2,3,4,5],[3,1,9,2,5],[1,3,1,6,7,1],[3,1,1,2,3,3,7,1],[2,1,6,3,6,1],
  [2,1,6,3,6,1],[2,2,1,6,4,6,1],[3,6,4,5,1],[5,4,4,6],[5,4,8,5,1,1],
  [3,2,6,3,2,1],[1,2,1,3,2,1],[1,2,2,3,3],[4,13,3],[11,2]]
  []

gchqPuzzle :: NonoPuzzle
gchqPuzzle = NonoPuzzle
  [ [7,3,1,1,7]
  , [1,1,2,2,1,1]
  , [1,3,1,3,1,1,3,1]
  , [1,3,1,1,6,1,3,1]
  , [1,3,1,5,2,1,3,1]
  , [1,1,2,1,1]
  , [7,1,1,1,1,1,7]
  , [3,3]
  , [1,2,3,1,1,3,1,1,2]
  , [1,1,3,2,1,1]
  , [4,1,4,2,1,2]
  , [1,1,1,1,1,4,1,3]
  , [2,1,1,1,2,5]
  , [3,2,2,6,3,1]
  , [1,9,1,1,2,1]
  , [2,1,2,2,3,1]
  , [3,1,1,1,1,5,1]
  , [1,2,2,5]
  , [7,1,2,1,1,1,3]
  , [1,1,2,1,2,2,1]
  , [1,3,1,4,5,1]
  , [1,3,1,3,10,2]
  , [1,3,1,1,6,6]
  , [1,1,2,1,1,2]
  , [7,2,1,2,5]
  ][
    [7,2,1,1,7]
  , [1,1,2,2,1,1]
  , [1,3,1,3,1,3,1,3,1]
  , [1,3,1,1,5,1,3,1]
  , [1,3,1,1,4,1,3,1]
  , [1,1,1,2,1,1]
  , [7,1,1,1,1,1,7]
  , [1,1,3]
  , [2,1,2,1,8,2,1]
  , [2,2,1,2,1,1,1,2]
  , [1,7,3,2,1]
  , [1,2,3,1,1,1,1,1]
  , [4,1,1,2,6]
  , [3,3,1,1,1,3,1]
  , [1,2,5,2,2]
  , [2,2,1,1,1,1,1,2,1]
  , [1,3,3,2,1,8,1]
  , [6,2,1]
  , [7,1,4,1,1,3]
  , [1,1,1,1,4]
  , [1,3,1,3,7,1]
  , [1,3,1,1,1,2,1,1,4]
  , [1,3,1,4,3,3]
  , [1,1,2,2,2,6,1]
  , [7,1,3,2,1,1]
  ]
  [(3,3),(3,4),(3,12),(3,13),(3,21),
   (8,6),(8,7),(8,10),(8,14),(8,15),(8,18),
   (16,6),(16,11),(16,16),(16,20),
   (21,3),(21,4),(21,9),(21,10),(21,15),(21,20),(21,21)
  ]


nonoSolve :: MonadSAT s m => NonoPuzzle -> m [[Bit]]
nonoSolve (NonoPuzzle rows cols extras) =

  do cells <- replicateM (length rows)
            $ replicateM (length cols) exists

     let check = match (===) . fmap bool . nonoHint
     assert $ and $ zipWith check rows cells
                 ++ zipWith check cols (transpose cells)
                 ++ map (\(r,c) -> cells !! r !! c) extras

     return cells

nonoHint :: [Int] -> RegExp Bool
nonoHint h =
  case h of
    [] -> blanks
    ns -> foldr next blanks
            $ blanks
            : intercalate gap (map thing ns)
  where
  blank   = fill False
  blanks  = RE (Rep blank)
  thing n = replicate n (fill True)
  gap     = [ blank, blanks ]

  fill     = RE . OneOf InSet . return
  next x y = RE (Seq (acceptsEmpty x && acceptsEmpty y) x y)
