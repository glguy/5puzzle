{-# Language RecordWildCards #-}
module Main where

import Booleans
import Control.Monad
import Data.Char
import Data.List (transpose)
import Data.Map (Map)
import Ersatz
import Prelude hiding ((&&),(||),all, any, and, or, not)
import SparseMap (SparseMap)
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import qualified SparseMap

------------------------------------------------------------------------
-- Board edges
------------------------------------------------------------------------

data Edge = Edge
  { edgeOn  :: Bit
  , edgeFwd :: Bit
  , edgeId  :: Bits
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

------------------------------------------------------------------------
-- Coordinates
------------------------------------------------------------------------

-- | Coordinates are column and row with origin in the top-left
type Coord = (Int,Int)

up, down, left, right :: Coord -> Coord
up    (x,y) = (x,y-1)
down  (x,y) = (x,y+1)
left  (x,y) = (x-1,y)
right (x,y) = (x+1,y)

------------------------------------------------------------------------
-- Board representation
------------------------------------------------------------------------

data Orient = Horiz | Vert
  deriving (Eq, Ord, Read, Show)

-- | Edges are labeled by association with the cell that the edge either
-- borders on top or left side.
--
-- @
-- +-
-- |?
-- @
type EdgeCoord = (Coord,Orient)

type Board = SparseMap EdgeCoord Edge

-- | Returns a list of edges that border the cell at the given coordinate.
--
-- @
-- +-+
-- |?|
-- +-+
-- @
clueNeighborhood :: Coord -> [EdgeCoord]
clueNeighborhood c =
  [            (c,Horiz),
    (c, Vert), {- c -} (right c,Vert),
               (down c,Horiz) ]

-- | Returns a list of edges that touch the intersection in
-- the top-left of a given cell coordinate. Each edge coordinate
-- is paired with a boolean that is 'true' for outgoing edges
-- and 'false' for incoming edges.
--
-- @
--   |
--   v
-- ->+->
--   |
--   v
-- @
edgeNeighborhood :: Boolean a => Coord -> [(EdgeCoord,a)]
edgeNeighborhood c =
  [                       ((up c, Vert), false)
  , ((left c, Horiz), false), {- + -} ((c, Horiz), true),
                          ((c, Vert), true) ]

-- | Generate the list of locations of all edges on a board of the
-- given width and height.
locations ::
  Int         {- ^ width              -} ->
  Int         {- ^ height             -} ->
  [EdgeCoord] {- ^ all edges on board -}
locations w h = body ++ bottomEdge ++ rightEdge
  where
    body       = [ ((x,y),o    ) | x <- [0..w-1], y <- [0..h-1], o <- [Horiz, Vert] ]
    bottomEdge = [ ((x,h),Horiz) | x <- [0..w-1] ]
    rightEdge  = [ ((w,y),Vert ) |                y <- [0..h-1] ]

------------------------------------------------------------------------
-- Puzzle representation
------------------------------------------------------------------------

-- | Puzzle parameters
data Puzzle = Puzzle Int Int (Map Coord Int) -- ^ width height clues

-- | Parse a puzzle from a text-file.
parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle w (length ls) cells

  where
    ls = lines str
    w  = case ls of
           x:_ -> length x
           []  -> 0
    cells = Map.fromList [ ((x,y),digitToInt n)
                         | (y,l) <- zip [0..] ls
                         , (x,n) <- zip [0..] l
                         , isDigit n ]

-- | Render the solution to a puzzle as ASCII art.
renderSolution ::
  Puzzle                   {- ^ puzzle parameters -} ->
  SparseMap EdgeCoord Bool {- ^ solution          -} ->
  String                   {- ^ rendering         -}
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

------------------------------------------------------------------------
-- Solver
------------------------------------------------------------------------

solvePuzzleIO :: Puzzle -> IO (Maybe (SparseMap EdgeCoord Bool))
solvePuzzleIO p =
  do result <- solveWith minisat (solvePuzzle p)
     case result of
       (Satisfied, Just x) -> return (Just x)
       (Unsatisfied, _)    -> return Nothing
       _                   -> fail "solvePuzzleIO: bad result"

solvePuzzle :: MonadSAT s m => Puzzle -> m (SparseMap EdgeCoord Bit)
solvePuzzle (Puzzle w h clues) =
  do (endId, cells) <- newCells w h

     -- only allow one edge to be the wrap-around edge
     assert (atMostOne (fmap (\e -> endId === edgeId e) cells))

     -- all edge IDs must be less than the wrap-around edge
     assert (all (\e -> edgeOn e ==> (edgeId e <=? endId)) cells)

     -- all intersections must be connected by sequentially numbered edges
     assert (and [ validateIntersection endId (x,y) cells
                 | x <- [0..w], y <- [0..h] ])

     -- all cell clues must be satisfied
     assert (and [ validateClue cells c n | (c,n) <- Map.toList clues ])

     -- returns map of active edges (forgetting edge IDs)
     return (fmap edgeOn cells)

-- | Returns 'true' when at most one element in the given list-like structure
-- is 'true'.
atMostOne :: (Foldable t, Boolean a) => t a -> a
atMostOne = not . snd . foldl aux (false,false)
  where
    aux (one,two) x = (one || x, two || one && x)

-- | Verify that the number in a particular cell matches the actual edges.
validateClue :: Board -> Coord -> Int -> Bit
validateClue board coord n = fromIntegral n === countBits localEdges
  where
    localEdges = [ edgeOn (SparseMap.index c board) | c <- clueNeighborhood coord ]

-- | Returns a list of the ways to partition a list of elements where
-- the first list has the given length and the second list is the
-- remaining elements.
partitions :: Int -> [a] -> [([a],[a])]
partitions 0 xs = return ([],xs)
partitions _ [] = []
partitions n (x:xs) =
  [ (x:l,r) | (l,r) <- partitions (n-1) xs ] ++
  [ (l,x:r) | (l,r) <- partitions n     xs ]

-- | Check that any intersection is unused in the solution or that it
-- is correct traversed by only two edges that are sequentially numbered.
validateIntersection :: Bits -> Coord -> Board -> Bit
validateIntersection endId coord board = emptyIntersection || activeIntersection
  where
    noLine (e,_) = not (edgeOn e)

    values = map (\ (c,d) -> (SparseMap.index c board, d)) (edgeNeighborhood coord)

    -- check that none of the edges entering this intersection are active
    emptyIntersection = all noLine values

    -- for any pair of edges that meet at this intersection check that
    -- the line enters along one edge and leaves along another such that
    -- the ID numbers of the edge are increasing.
    activeIntersection =
      or [ all noLine others && edgeOn edge1 && edgeOn edge2 &&
           (connected edge1 out1 edge2 out2 || connected edge2 out2 edge1 out1)
         | ([(edge1,out1),(edge2,out2)],others) <- partitions 2 values
         ]

    -- checks that given two edges and flags indicating the direction
    -- of the edges if the smaller number is entering this intersection
    -- and the larger number is leaving it.
    connected edge1 out1 edge2 out2 =
      edgeFwd edge1 /== out1 && edgeFwd edge2 === out2 &&
      (edgeId edge1 + 1 === edgeId edge2 ||
       edgeId edge1 === endId && edgeId edge2 === 0)

-- | Generate a new board containing the edges available on a board
-- with the given width and height. Also returns the number suitable
-- for representing the largest edge ID.
newCells ::
  MonadSAT s m =>
  Int {- ^ width  -} ->
  Int {- ^ height -} ->
  m (Bits, Board)
newCells w h =
  do let keys       = locations w h
         bitsNeeded = ceiling (logBase 2 (fromIntegral (length keys)) :: Double)
     cells <- sequence
              (SparseMap.fromList (pure noEdge) [ (k,newEdge bitsNeeded) | k <- keys ])
     endId <- newBits bitsNeeded
     return (endId, cells)

main :: IO ()
main =
  do puzzle <- parsePuzzle <$> getFile
     res <- solvePuzzleIO puzzle
     case res of
       Just solution -> putStr (renderSolution puzzle solution)
       Nothing ->
         do putStrLn "No solution possible!"
            putStr (renderSolution puzzle (SparseMap.constant false))

-- | Returns either the file specified in the command arguments, stdin
-- contents if @-@ is specified, or fails with an error message.
getFile :: IO String
getFile =
  do args <- getArgs
     case args of
       ["-"] -> getContents
       [fn] -> readFile fn
       _    -> do hPutStrLn stderr "Usage: Loopy FILENAME"
                  exitFailure
