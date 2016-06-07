{- 12 coins puzzle.

You are given 12 coins and a balance scale. You are told that 11 of the coins
weight exactly the same as each other. 1 of the coins has a different weight
than the rest. You can take 3 measurements using the balance scale. Plan a
set of weighings that will enable you to find the odd coin as well as if it
is lighter or heavier than the rest.

 -}
{-# Language DeriveGeneric, DeriveTraversable #-}
{-# Language TypeFamilies #-}
module Main (main) where

import Ersatz
import Booleans
import Select
import Control.Monad
import GHC.Generics
import Data.List (elemIndices)
import Data.Word (Word8)
import Prelude hiding (or,(&&),(||),all,not)

------------------------------------------------------------------------

data Decision a = Answer a | Decision { lo_hi, hi_lo, same :: Decision a }
  deriving (Show, Functor, Foldable, Traversable)

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

data Weight = Light | Heavy deriving (Show, Eq, Generic)
instance Equatable Weight

problem :: MonadSAT s m => m ([Partition Bit], Decision (Select Weight,Bit4))
problem =
  do ps <- replicateM 3 (partitionExists 12)

     let buildDecision m = Decision <$> m <*> m <*> m
         answer          = (,) <$> selectList [Light,Heavy] <*> exists
     tree <- buildDecision $ buildDecision $ buildDecision $ Answer <$> answer

     assert $ flip all [0..11] $ \i ->
              flip all [Light,Heavy] $ \mode ->
              check i mode ps tree

     return (ps,tree)

-- | Check that the partitionings of coins and the decision tree
-- correctly identify the identity of a given coin and weight mode.
check :: Int -> Weight -> [Partition Bit] -> Decision (Select Weight,Bit4) -> Bit
check i weight = go
  where
  heavy = case weight of
            Heavy -> true
            Light -> false

  go [] (Answer a) = encode (weight, fromIntegral i)===a

  go (Partition l r : ps) d@Decision{}
     = pick_lo_hi && go ps (lo_hi d)
    || pick_hi_lo && go ps (hi_lo d)
    || pick_same  && go ps (same  d)
    where
    pick_lo_hi = choose inR inL heavy
    pick_hi_lo = choose inL inR heavy
    pick_same  = not inL && not inR
    inL = l!!i
    inR = r!!i

  go _ _ = false

-- | Compute the 3 partitions and the decision tree that can uniquely
-- identify any one coin that is lighter or heavier than all the rest.
run :: IO (Decision (Weight, Word8))
run =
  do (Satisfied, Just (ps,tree)) <- solveWith minisat problem
     mapM_ (print . prettyPartition) ps
     return tree

main :: IO ()
main = print =<< run
