{-# Language ConstraintKinds #-}

module Booleans (MonadSAT, existsNat, coverOne, unique, checking, isTrue, exactlyOne) where

import Ersatz
import Data.List (tails, mapAccumL)
import Data.Foldable (toList)
import Control.Monad.State (MonadState)
import Control.Monad (replicateM)
import Prelude hiding (not, all, (&&), (||))

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
