{-# Language ConstraintKinds #-}

module Booleans
  ( MonadSAT
  , existsNat
  , coverOne
  , unique
  , checking
  , isTrue
  , exactlyOne
  , any2, all2
  , countBits
  , chooseBits
  ) where

import Ersatz
import Data.List (tails, mapAccumL)
import Data.Foldable (toList)
import Control.Monad.State (MonadState)
import Control.Monad (replicateM)
import Prelude hiding (not, all, (&&), (||), and, or)

-- | Returns a summary value of where a boolean is true in exactly
-- one position in the list.
exactlyOne :: (Foldable t, Boolean a) => t a -> a
exactlyOne xs = allCovered && nor overlaps
  where
  (allCovered, overlaps) = mapAccumL addMask false (toList xs)

  addMask covered mask = (covered || mask, covered && mask)

isTrue :: (Equatable a, Boolean a) => a -> Bit
isTrue x = true === x

checking :: (HasSAT s, MonadState s m) => m a -> (a -> Bit) -> m a
checking m p =
  do x <- m
     assert (p x)
     return x

type MonadSAT s m = (HasSAT s, MonadState s m)

unique :: (Foldable t, Equatable a) => t a -> Bit
unique = all unique1 . tails . toList
  where
  unique1 []     = true
  unique1 (y:ys) = all (/==y) ys

-- | Returns a summary value of where a boolean is true in exactly
-- one position in the list.
coverOne :: (Equatable a, Boolean a) => a -> [a] -> Bit
coverOne mask []     = mask === false
coverOne mask (x:xs) = false === (not mask && x) && coverOne (mask && not x) xs

existsNat :: MonadSAT s m => Integer -> m Bits
existsNat limit =
  do x <- Bits <$> replicateM (log2 0 (limit-1)) exists
     assert (x <? encode limit)
     return x
  where
  log2 acc 0 = acc
  log2 acc n = log2 (acc+1) (n`quot`2)

-- | Zip two lists of the same length together with a function.
-- Return true when all results are true.
all2 :: (a -> b -> Bit) -> [a] -> [b] -> Bit
all2 f xs ys = and (zipWith f xs ys)

-- | Zip two lists of the same length together with a function.
-- Return true when any result is true.
any2 :: (a -> b -> Bit) -> [a] -> [b] -> Bit
any2 f xs ys = or (zipWith f xs ys)

-- | Count the number of 'true' elements in a list of bits.
countBits :: [Bit] -> Bits
countBits = sumBits . map (Bits . return)


-- | Returns a value equal to the first argument when the selector is false
-- and equal to the second argument when the selector is true.
chooseBits ::
  Bits {- ^ false case -} ->
  Bits {- ^ true  case -} ->
  Bit  {- ^ selector   -} ->
  Bits
chooseBits (Bits x0) (Bits y0) b = Bits (merge x0 y0)
  where
    merge []     ys     = map (    b &&) ys
    merge xs     []     = map (not b &&) xs
    merge (x:xs) (y:ys) = choose x y b : merge xs ys
