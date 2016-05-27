module Main where

import Ersatz
import Booleans
import Linear
import Select
import SparseMap
import Control.Applicative
import Data.List (nub, permutations)
import Data.Traversable (for)
import Control.Monad
import Data.Foldable(for_)
import Prelude hiding ((&&), (||), not, all, any,or,not)

data Piece = Piece Int Int Int    deriving (Eq, Show)
type Coord = V3 Int

-- | Compute all the unique orientations of a piece along each
-- of the 3 axes.
orientations :: Piece -> [Piece]
orientations (Piece x y z) =
  nub [ Piece x' y' z' | [x',y',z'] <- permutations [x,y,z] ]

blank, cube, flat, thick :: Piece
cube  = Piece 1 1 1
flat  = Piece 4 2 1
thick = Piece 2 2 3
blank = Piece 0 0 0

-- | Compute a bitmap of the locations that a piece covers
-- when placed at a particular location
cover :: Coord -> Piece -> SparseMap (V3 Int) Bit
cover (V3 x y z) (Piece dx dy dz) =
  trueList (liftA3 V3 [x..x+dx-1] [y..y+dy-1] [z..z+dz-1])

locations :: [Coord]
locations = liftA3 V3 [0..4] [0..4] [0..4]

selectOrientation :: MonadSAT s m => Select Piece -> m (Select Piece)
selectOrientation x = join <$> traverse (selectList . orientations) x

solution :: MonadSAT s m => m [Select Piece]
solution  =
  do (pieces, orientedPieces, coverMaps)
        <- unzip3 <$>
           for locations (\loc ->
             do p  <- selectList [blank,cube,flat,thick]
                po <- selectOrientation p
                let pcov = runSelectWith (cover loc) po
                return (p,po,pcov))

     assert $ coverOne (trueList locations) coverMaps
     assert $ count cube  pieces === 5
     assert $ count thick pieces === 6
     assert $ count flat  pieces === 6
     assert $ count blank pieces === (125-6-6-5)

     return orientedPieces

count :: Eq a => a -> [Select a] -> Bits
count target xs = sumBits [ Bits [eltToBit p] | p <- xs ]
  where
  eltToBit = runSelectWith $ \x -> bool (x == target)

main :: IO ()
main =
  do (Satisfied, Just res) <- solveWith minisat solution
     putStrLn "color(\"BurlyWood\",0.8){"
     for_ (zip locations res) $ \(V3 x y z, piece) ->
        case piece of
          Piece 0 0 0 -> return ()
          Piece dx dy dz ->
            putStrLn $ "translate(" ++ show (map recenter [x,y,z]) ++ "){"
                    ++ "cube(" ++ show (map gap [dx,dy,dz]) ++ ");}"
     putStrLn "}"
  where
  gap, recenter :: Int -> Double
  gap x = fromIntegral x - 0.2
  recenter x = fromIntegral x - 2.5
