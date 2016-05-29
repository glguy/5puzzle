{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module RegExp.AST
  ( RegF(..), RegExp(..), SetMode(..),
    empty, one, oneOf, noneOf, anyone, (|||), (>>>), rep, view,
    grouping, foldRegExp,
    AcceptsEmpty(..)
  ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

data SetMode = InSet | NotInSet
  deriving (Read,Show,Eq,Ord)

data RegF a r
  = Empty
  | OneOf SetMode [a]
  | Rep r
  | Group Bool r
  | Alt Bool r r -- the Bool caches if accept empty
  | Seq Bool r r -- ditto
    deriving (Read,Show,Functor,Foldable,Traversable)


class AcceptsEmpty t where
  acceptsEmpty :: t -> Bool

instance AcceptsEmpty (RegF a r) where
  acceptsEmpty r =
    case r of
      Empty     -> True
      OneOf {}  -> False
      Rep {}    -> True
      Group b _ -> b
      Alt b _ _ -> b
      Seq b _ _ -> b


newtype RegExp a = RE (RegF a (RegExp a))
                      deriving (Read,Show)

instance Functor RegExp where
  fmap f r = RE $
    case view r of
      Empty     -> Empty
      OneOf m x -> OneOf m (map f x)
      Rep p     -> Rep (fmap f p)
      Group b p -> Group b (fmap f p)
      Alt b p q -> Alt b (fmap f p) (fmap f q)
      Seq b p q -> Seq b (fmap f p) (fmap f q)

foldRegExp :: (RegF a r -> r) -> RegExp a -> r
foldRegExp f (RE x) = f (fmap (foldRegExp f) x)

instance AcceptsEmpty (RegExp a) where
  acceptsEmpty (RE e) = acceptsEmpty e

empty    :: RegExp a
empty     = RE Empty

one      :: a -> RegExp a
one a     = RE (OneOf InSet [a])

oneOf    :: [a] -> RegExp a
oneOf xs  = RE (OneOf InSet xs)

noneOf   :: [a] -> RegExp a
noneOf xs = RE (OneOf NotInSet xs)

anyone   :: RegExp a
anyone    = noneOf []

(|||)    :: RegExp a -> RegExp a -> RegExp a
r1 ||| r2 = RE (Alt (acceptsEmpty r1 || acceptsEmpty r2) r1 r2)

(>>>)    :: RegExp a -> RegExp a -> RegExp a
r1 >>> r2 = RE (Seq (acceptsEmpty r1 && acceptsEmpty r2) r1 r2)

rep      :: RegExp a -> RegExp a
rep r     = RE (Rep r)

grouping :: RegExp a -> RegExp a
grouping r = RE (Group (acceptsEmpty r) r)

view     :: RegExp a -> RegF a (RegExp a)
view (RE e) = e
