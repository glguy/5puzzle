{-# Language ConstraintKinds #-}

module Booleans (MonadSAT, unique, checking, isTrue, exactlyOne) where

import Ersatz
import Data.List (tails, mapAccumL)
import Control.Monad.State (MonadState)
import Prelude hiding (all, (&&), (||))

-- | Returns a summary value of where a boolean is true in exactly
-- one position in the list.
exactlyOne :: Boolean a => [a] -> a
exactlyOne xs = allCovered && nor overlaps
  where
  (allCovered, overlaps) = mapAccumL addMask false xs

  addMask covered mask = (covered || mask, covered && mask)

isTrue :: (Equatable a, Boolean a) => a -> Bit
isTrue x = true === x

checking :: (HasSAT s, MonadState s m) => m a -> (a -> Bit) -> m a
checking m p =
  do x <- m
     assert (p x)
     return x

type MonadSAT s m = (HasSAT s, MonadState s m)

unique :: Equatable a => [a] -> Bit
unique = all unique1 . tails
  where
  unique1 []     = true
  unique1 (y:ys) = all (/==y) ys
