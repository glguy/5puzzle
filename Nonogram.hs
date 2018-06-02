module Main where

import RegExp.AST
import RegExp.Match     (match)

import Ersatz
import Booleans

import Control.Monad
import Data.List (intercalate,transpose)
import Prelude hiding ((&&),(||),all,and,any,or,not)

import Data.Colour.SRGB
import Diagrams.Prelude (Diagram, mkSizeSpec, scale, fc, square)
import Diagrams.TwoD.Transform (translateX, translateY)
import Diagrams.Backend.SVG (B, renderSVG)

main :: IO ()
main =
  do Just xs <- getModel (nonoSolve nonoPuzzle2)
     let asChar True  = '*'
         asChar False = ' '
     putStr $ unlines $ map (map asChar) xs

     let sizeSpec = mkSizeSpec (pure Nothing)
     renderSVG "output.svg" sizeSpec (drawSolution xs)

drawSolution :: [[Bool]] -> Diagram B
drawSolution rows
  = scale 10
  $ foldMap drawCell [ (r,c) | (r,row ) <- zip [0..] rows
                             , (c,True) <- zip [0..] row ]

drawCell :: (Int,Int) -> Diagram B
drawCell (r,c)
  = translateX (fromIntegral c)
  $ translateY (fromIntegral (negate r))
  $ fc (sRGB24 30 30 30)
  $ square 1

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
  ]
  [ [7,2,1,1,7]
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

     let check = match (===) . nonoHint
     assert $ all2 check rows cells
     assert $ all2 check cols (transpose cells)
     assert $ all (\(r,c) -> cells !! r !! c) extras

     return cells

nonoHint :: Boolean a => [Int] -> RegExp a
nonoHint = foldr next blanks . intercalate [blank] . map hint
  where
  blank  = fill false
  hint n = blanks : replicate n (fill true)

  blanks   = RE (Rep blank)
  fill x   = RE (OneOf InSet [x])
  next x y = RE (Seq (acceptsEmpty x && acceptsEmpty y) x y)
