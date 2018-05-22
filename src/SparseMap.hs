{-# Language TypeFamilies #-}
module SparseMap
  (SparseMap(..)
  , trueList
  , falseList
  , fromList
  , index
  , insert
  , constant) where

import Ersatz
import ChooseBit
import Prelude hiding ((&&), (||), not)
import Control.Applicative
import Data.Monoid ((<>))

import           Data.Map (Map)
import qualified Data.Map as Map

data SparseMap k v = SparseMap !(Map k v) v
  deriving (Read, Show)

instance (Ord k, ChooseBit v) => ChooseBit (SparseMap k v) where
  chooseBit x y b = (\l r -> chooseBit l r b) <$> x <*> y

instance Functor (SparseMap k) where
  fmap f (SparseMap m x) = SparseMap (f <$> m) (f x)

instance Foldable (SparseMap k) where
  foldMap f (SparseMap m x) = foldMap f m <> f x

instance Traversable (SparseMap k) where
  traverse f (SparseMap m x) = SparseMap <$> traverse f m <*> f x

instance Ord k => Applicative (SparseMap k) where
  pure = constant
  SparseMap fs f <*> SparseMap xs x = SparseMap merged (f x)
    where
    merged = Map.mergeWithKey
                  (\_ g y -> Just (g y))
                  (($ x) <$>)
                  (f <$>)
                  fs xs

fromList :: Ord k => v -> [(k,v)] -> SparseMap k v
fromList def xs = SparseMap (Map.fromList xs) def

trueList :: (Boolean v, Ord k) => [k] -> SparseMap k v
trueList xs = SparseMap (Map.fromList [ (x,true) | x <- xs ]) false

falseList :: (Boolean v, Ord k) => [k] -> SparseMap k v
falseList xs = SparseMap (Map.fromList [ (x,false) | x <- xs ]) true

index :: Ord k => k -> SparseMap k v -> v
index i (SparseMap m x) = Map.findWithDefault x i m

insert :: Ord k => k -> v -> SparseMap k v -> SparseMap k v
insert k v (SparseMap m x) = SparseMap (Map.insert k v m) x

foldSparseMap :: (v -> v -> v) -> SparseMap k v -> v
foldSparseMap f (SparseMap m x) = foldr f x m

instance (Equatable v, Ord k) => Equatable (SparseMap k v) where
  x === y = foldSparseMap (&&) (liftA2 (===) x y)

instance (Ord k, Boolean v) => Boolean (SparseMap k v) where
  bool = constant . bool
  (&&) = liftA2 (&&)
  (||) = liftA2 (||)
  xor  = liftA2 xor
  not  = fmap not
  any f = foldr (\y ys -> f y || ys) false
  all f = foldr (\y ys -> f y && ys) true

constant :: v -> SparseMap k v
constant = SparseMap Map.empty

instance Codec v => Codec (SparseMap k v) where
  type Decoded (SparseMap k v) = SparseMap k (Decoded v)
  encode = fmap encode
  decode sol = traverse (decode sol)
