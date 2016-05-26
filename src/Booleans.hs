{-# Language ConstraintKinds #-}

module Booleans (MonadSAT, unique, checking, isTrue, exactlyOne) where

import Ersatz
import Data.List (tails, mapAccumL)
import Data.Foldable (toList)
import Control.Monad.State (MonadState)
import Prelude hiding (all, (&&), (||))

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
