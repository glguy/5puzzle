{-|
Module      : Main
Description : Solver for the Range puzzle
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/range.html

-}
module Main where

import           Prelude hiding ((&&), (||), and, not, or, any, all)
import           Data.Foldable (foldl')
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

import           Booleans
import           BoxDrawing
import           Coord
import           Ersatz

-- | Puzzle specification
data Puzzle = Puzzle Int Int (Map Coord Int) -- ^ width height clues
  deriving (Show)


-- | Parse a puzzle from a text-file.
parsePuzzle :: String -> Puzzle
parsePuzzle str = Puzzle w h cells
  where
    ls    = map words (lines str)
    h     = length ls
    w     = maximum (0 : map length ls)
    cells = Map.fromList
              [ (C x y,n)
                | (y,l) <- zip [0..] ls
                , (x,c) <- zip [0..] l
                , Just n <- [readMaybe c] ]

-- | Render a puzzle solution
renderSolution ::
  Puzzle         {- ^ puzzle parameters -} ->
  Map Coord Bool {- ^ marks             -} ->
  String
renderSolution (Puzzle w h clues) marks =
  renderGrid w h edge cell
   where
     edge (C x y) o
       | 0 == x && o == Vert  ||
         0 == y && o == Horiz ||
         x == w || y == h = Just Double
       | otherwise = Just Thin
     cell c
       | Map.findWithDefault False c marks = "██"
       | otherwise = maybe " " show (Map.lookup c clues)

-- | Generate a mark assignment that satisfies the clues and also
-- the no orthogonal marks rule. The connectivity rule is checked
-- as a post-processing step.
solutionExists :: MonadSAT s m => Puzzle -> m (Map Coord Bit)
solutionExists (Puzzle w h clues) =
  do let locations = C <$> [0..w-1] <*> [0..h-1]
     marks <- sequence
            $ Map.fromList
                [ (c, if Map.member c clues then pure false else exists)
                | c <- locations ]

     let isMarked c = Map.findWithDefault false c marks
     assert $ and [ not (isMarked c && isMarked d)
                    | c <- locations
                    , d <- [ right c, down c ] ]

     assert $ and [ checkClue w h marks c n
                    | (c,n) <- Map.toList clues ]

     return marks

-- | Count the number of false bits in the beginning of the list
visible :: [Bit] -> Bits
visible = countBits . map not . scanl1 (||)

checkClue ::
  Int           {- ^ width         -} ->
  Int           {- ^ height        -} ->
  Map Coord Bit {- ^ marks         -} ->
  Coord         {- ^ clue location -} ->
  Int           {- ^ clue          -} ->
  Bit           {- ^ success       -}
checkClue w h marks (C x y) n = total === fromIntegral (n-1)
  where
    total  = sum (visible . toBits <$> [u,d,l,r])
    toBits = map (marks Map.!)
    u = [C x y | y <- [y-1, y-2 .. 0  ] ]
    d = [C x y | y <- [y+1, y+2 .. h-1] ]
    l = [C x y | x <- [x-1, x-2 .. 0  ] ]
    r = [C x y | x <- [x+1, x+2 .. w-1] ]

-- | Solve the puzzle by repeatedly calling the solver until
-- the given solution is fully connected.
solvePuzzle :: Puzzle -> IO (Maybe (Map Coord Bool))
solvePuzzle puzzle = loop []
  where
    loop boundaries =
      do res <- getModel (solutionExists puzzle `checking` \s ->
                           all (not . all (s Map.!)) boundaries)
         case res of
           Just s -> case getBoundary s of
                       Nothing -> return (Just s)
                       Just b  -> loop (b : boundaries)
           Nothing -> return Nothing

main :: IO ()
main =
  do puzzle <- parsePuzzle <$> getFile
     result <- solvePuzzle puzzle
     case result of
       Just solution ->
         do putStr (renderSolution puzzle solution)
       Nothing ->
         do putStrLn "No solution possible!"
            putStr (renderSolution puzzle Map.empty)

-- | Returns either the file specified in the command arguments, stdin
-- contents if @-@ is specified, or fails with an error message.
getFile :: IO String
getFile =
  do args <- getArgs
     case args of
       ["-"] -> do getContents
       [fn ] -> do readFile fn
       _     -> do hPutStrLn stderr "Usage: Loopy FILENAME"
                   exitFailure

-- | Returns the coordinates of the marks that divide two regions
-- of unmarked cells, if such a set of marks exists.
getBoundary :: Map Coord Bool -> Maybe [Coord]
getBoundary marks
  | Set.null g = Nothing
  | otherwise  = Just (groupBoundary marks g)
  where
    g = getIsolatedGroup (Map.keysSet (Map.filter not marks))

-- | Given the map of marks and a known connected region
-- of unmarked cells, find the list of coordinates that
-- separates this region from other regions.
groupBoundary ::
  Map Coord Bool {- ^ marks          -} ->
  Set Coord      {- ^ isolated group -} ->
  [Coord]        {- ^ boundary marks -}
groupBoundary marks g =
  [ c
    | (c, True) <- Map.toList marks -- consider all marks
    , let adj = [ Set.member d g
                    | d <- cardinalNeighbors c
                    , Just False == Map.lookup d marks
                    ]
    , or adj      -- adjacent to isolated region of cells
    , any not adj -- adjacent to remaining unmarked cells
    ]

-- | Return set of of coordinates that are isolated from
-- other coordinates in the input set.
getIsolatedGroup :: Set Coord -> Set Coord
getIsolatedGroup coords =
  case Set.minView coords of
    Nothing        -> Set.empty
    Just (root, _) -> visit coords root
  where
    visit avail x
      | Set.member x avail = foldl' visit (Set.delete x avail) (cardinalNeighbors x)
      | otherwise          = avail
