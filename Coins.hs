{- 12 coins puzzle.

You are given 12 coins and a balance scale. You are told that 11 of the coins
weight exactly the same as each other. 1 of the coins has a different weight
than the rest. You can take 3 measurements using the balance scale. Plan a
set of weighings that will enable you to find the odd coin.

 -}
{-# Language DeriveGeneric, DeriveTraversable #-}
{-# Language TypeFamilies #-}
module Main (main) where

import Ersatz
import Booleans
import FromBit
import Control.Monad
import GHC.Generics
import Data.List (elemIndices)
import Data.Word (Word8)
import Prelude hiding (or,(&&),(||),all,not)

------------------------------------------------------------------------

data Decision a = Decision { lo_hi, hi_lo, same :: a }
  deriving (Show, Functor, Foldable, Traversable)

type Decision3 a = Decision (Decision (Decision a))

instance Codec a => Codec (Decision a) where
  type Decoded (Decision a) = Decision (Decoded a)
  encode = fmap encode
  decode = traverse . decode

------------------------------------------------------------------------

data Partition a = Partition [a] [a]
  deriving (Show, Functor, Foldable, Traversable, Generic)

-- | Construct a partition of @n@ elements where each element
-- is in at most one of the two, but not both, and both sides
-- have the same number of elements.
partitionExists :: MonadSAT s m => Int -> m (Partition Bit)
partitionExists n =
  do x <- replicateM n exists
     y <- replicateM n exists
     assert (nor (zipWith (&&) x y))
     assert (countBits x === countBits y)
     return (Partition x y)

-- | Find the indexes that are set on the left and right of the partition
prettyPartition :: Partition Bool -> ([Int],[Int])
prettyPartition (Partition x y) = (elemIndices True x, elemIndices True y)

instance Codec a => Codec (Partition a) where
  type Decoded (Partition a) = Partition (Decoded a)
  encode = fmap encode
  decode = traverse . decode

------------------------------------------------------------------------

data Mode = Light | Heavy
  deriving (Show, Eq)

problem :: MonadSAT s m => m (Partition Bit, Partition Bit, Partition Bit, Decision3 Bit4)
problem =
  do [d1,d2,d3] <- replicateM 3 (partitionExists 12)

     let buildDecision m = Decision <$> m <*> m <*> m
     tree <- buildDecision $ buildDecision $ buildDecision exists

     assert $ flip all [0..11] $ \i ->
              flip all [Light,Heavy] $ \mode ->
              check d1 d2 d3 tree i mode

     return (d1,d2,d3,tree)

-- | Check that the three partitionings of coins and the decision tree
-- correctly identify the identity of a given coin and weight mode.
check ::
  Partition Bit -> Partition Bit -> Partition Bit ->
  Decision3 Bit4 -> Int -> Mode -> Bit
check p1 p2 p3 d i mode = encode (fromIntegral i) === applyPartitions d
  where
  applyPartitions
    =             (select p1 i mode)
    . fmap        (select p2 i mode)
    . (fmap.fmap) (select p3 i mode)

-- | Given a partitioning of the coins and a coin that is lighter
-- or heavier than the rest of the coins, choose the corresponding
-- element in the decision tree.
select :: FromBit a => Partition Bit -> Int -> Mode -> Decision a -> a
select (Partition l r) i m d
   = fromBit (inL && heavy || inR && not heavy) && lo_hi d
  || fromBit (inR && heavy || inL && not heavy) && hi_lo d
  || fromBit (not inL && not inR)               && same d
  where
  inL = l!!i
  inR = r!!i
  heavy = case m of
            Heavy -> true
            Light -> false

-- | Compute the 3 partitions and the decision tree that can uniquely
-- identify any one coin that is lighter or heavier than all the rest.
run :: IO (Decision3 Word8)
run =
  do (Satisfied, Just (p1,p2,p3,tree)) <- solveWith minisat problem
     print (prettyPartition p1)
     print (prettyPartition p2)
     print (prettyPartition p3)
     return tree

main :: IO ()
main = print =<< run
