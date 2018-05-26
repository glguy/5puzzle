{-# Language RecordWildCards #-}
module Main where

import Data.Char
import Prelude hiding ((&&),(||),all, any, and, not)
import Ersatz
import Booleans
import Data.Map (Map)
import Data.List (transpose)
import Control.Monad
import Data.Traversable (mapAccumL)
import Data.Foldable (toList)
import qualified Data.Map as Map
import SparseMap (SparseMap)
import qualified SparseMap
import Debug.Trace

type Coord = (Int,Int)

data Orient = Horiz | Vert
  deriving (Eq, Ord, Read, Show)

data Edge = Edge
  { edgeOn :: Bit
  , edgeFwd :: Bit
  , edgeId :: Bits
  }

noEdge :: Edge
noEdge = Edge false false 0

newEdge :: MonadSAT s m => Int -> m Edge
newEdge n =
  do edgeOn  <- exists
     edgeFwd <- exists
     edgeId  <- newBits n
     return Edge{..}

newBits :: MonadSAT s m => Int -> m Bits
newBits n = Bits <$> replicateM n exists

type Board = SparseMap (Coord,Orient) Edge

data Puzzle = Puzzle Int Int (Map Coord Int)

example =
  "  3  2 \n\
  \2   33 \n\
  \  2    \n\
  \  311  \n\
  \2    11\n\
  \222  2 \n\
  \  3 101\n"

example2 =
  "  32323\n\
  \       \n\
  \ 122222\n\
  \3  32 2\n\
  \3011  1\n\
  \3 33 1 \n\
  \     22\n"

parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle w (length ls)
                     (Map.fromList [ ((x,y),digitToInt n)
                                     | (y,l) <- zip [0..] ls
                                     , (x,n) <- zip [0..] l
                                     , isDigit n ])

  where
    ls = lines str
    w = case ls of
          x:_ -> length x
          []  -> 0

renderSolution :: Puzzle -> SparseMap (Coord, Orient) Bool -> String
renderSolution (Puzzle w h clues) solution =
  unlines $ concat
  [ map concat $ transpose
     [ [['+', if nh then '-' else ' ']
      ,[if nv then '|' else ' ', maybe ' ' intToDigit (Map.lookup (x,y) clues)]]
     | x <- [0..w]
     , let nh = SparseMap.index ((x,y),Horiz) solution
     , let nv = SparseMap.index ((x,y),Vert ) solution
        ]
     | y <- [0..h] ]


main = print ()

solvePuzzleIO :: Puzzle -> IO (Maybe (SparseMap (Coord, Orient) Bool))
solvePuzzleIO p =
  do result <- solveWith minisat (solvePuzzle p)
     return $! case result of
       (Satisfied, x) -> x
       _              -> Nothing

solvePuzzle :: MonadSAT s m => Puzzle -> m (SparseMap (Coord,Orient) Bit)
solvePuzzle p@(Puzzle w h clues) =
  do (endId, cells) <- newCells p

     assert (atMostOne (fmap (\e -> endId === edgeId e) cells))

     assert (all (\e -> edgeOn e ==> (edgeId e <=? endId)) cells)

     assert $ and [ validateIntersection endId (x,y) cells
                  | x <- [0..w], y <- [0..h] ]

     assert $ and [ validateClue cells c n | (c,n) <- Map.toList clues ]

     return (fmap edgeOn cells)

atMostOne :: (Foldable t, Boolean a) => t a -> a
atMostOne = not . snd . foldl (\ (one,two) x -> (one || x, two || one && x)) (false,false)

validateClue :: Board -> Coord -> Int -> Bit
validateClue board coord n =
  fromIntegral n === countBits [ edgeOn (SparseMap.index c board) | c <- clueNeighborhood coord ]

clueNeighborhood :: Coord -> [(Coord,Orient)]
clueNeighborhood (x,y) =
  [                ((x,y),Horiz),
    ((x,y), Vert),   {- ? -}      ((x+1,y),Vert),
                  ((x,y+1),Horiz) ]

partitions :: Int -> [a] -> [([a],[a])]
partitions 0 xs = return ([],xs)
partitions _ [] = []
partitions n (x:xs) =
  [ (x:l,r) | (l,r) <- partitions (n-1) xs ] ++
  [ (l,x:r) | (l,r) <- partitions n     xs ]

validateIntersection :: Bits -> Coord -> Board -> Bit
validateIntersection endId coord board =
    all noLine values ||
    any (\(((edge1,e1),(edge2,e2)),others) ->
       all noLine others && edgeOn edge1 && edgeOn edge2 &&

       (edgeFwd edge1 /== e1 && edgeFwd edge2 === e2 &&
          (edgeId edge1 + 1 === edgeId edge2 ||
           edgeId edge1 === endId && edgeId edge2 === 0)


        )) [ (xy,z) | ([x,y],z) <- partitions 2 values, xy <- [(x,y),(y,x)] ]
  where
    noLine (e,_) = not (edgeOn e)

    values = map (\ (c,d) -> (SparseMap.index c board, d)) (edgeNeighborhood coord)

edgeNeighborhood :: Boolean a => Coord -> [((Coord,Orient),a)]
edgeNeighborhood (x,y) =
  [                       (((x,y-1), Vert), false)
  , (((x-1, y), Horiz), false)   {- + -}  , (((x,y), Horiz), true)
                        , (((x,y), Vert), true) ]

newCells :: MonadSAT s m => Puzzle -> m (Bits, Board)
newCells (Puzzle w h _) =
  do let keys = locations w h
         bitsNeeded = ceiling (logBase 2 (fromIntegral (length keys)))
     cells <- sequence
              (SparseMap.fromList (pure noEdge) [ (k,newEdge bitsNeeded) | k <- keys ])
     endId <- newBits bitsNeeded
     return (endId, cells)

locations :: Int -> Int -> [ (Coord, Orient) ]
locations w h = body ++ bottomEdge ++ rightEdge
  where
    body = [ ((x,y),o) | x <- [0..w-1], y <- [0..h-1], o <- [Horiz, Vert] ]
    bottomEdge = [ ((x,h),Horiz) | x <- [0..w-1] ]
    rightEdge    = [ ((w,y),Vert) | y <- [0..h-1] ]
