{-# Language TypeFamilies #-}
module Select
  ( Select
  , runSelect
  , runSelectWith
  , select
  , selectList
  , mergeSelects
  , foldSelect
  , selectEq
  ) where

import Ersatz
import FromBit
import Data.Maybe
import Data.List (findIndex)
import Data.List.NonEmpty
import Data.Semigroup
import Control.Applicative
import Control.Monad.State
import Prelude hiding (and, or, (&&), (||) ,not)

-- | A set of choices and an index of the chosen element of that set
data Select a = Selected a | Choice Bit (Select a) (Select a)

-- | Symbolic selection from a non-empty list of alternatives.
selectList :: (MonadState s m, HasSAT s) => [a] -> m (Select a)
selectList []     = error "selectList: empty list"
selectList (x:xs) = select (x :| xs)
{-# SPECIALIZE selectList :: [a] -> StateT SAT IO (Select a) #-}

-- | Symbolic selection from a non-empty list of alternatives.
select :: (MonadState s m, HasSAT s) => NonEmpty a -> m (Select a)
select = mergeSelects . fmap Selected

mergeSelects :: (MonadState s m, HasSAT s) => NonEmpty (Select a) -> m (Select a)
mergeSelects (x  :| [])      = return x
mergeSelects (x1 :| x2 : xs) =
  do b   <- exists
     xs' <- reduce xs b
     mergeSelects (Choice b x1 x2 :| xs')
  where
  reduce (x1:x2:xs) b =
     do xs' <- reduce xs b
        return (Choice b x1 x2 : xs')
  reduce xs _ = return xs

runSelect :: FromBit a => Select a -> a
runSelect s = or (aux true s [])
  where
  aux sel (Selected x) z = (fromBit sel && x) : z
  aux sel (Choice b x y) z =
    aux (sel && not b) x (aux (sel && b) y z)

-- This is cleaner but it seems to run more slowly, presumably
-- due to doing more of the boolean operations at 'a' instead of doing
-- them at 'Bit'
-- runSelect :: FromBit a => Select a -> a
-- runSelect (Selected x)   = x
-- runSelect (Choice b x y) = choose (runSelect x) (runSelect y) (fromBit b)

instance Codec (Select a) where

  type Decoded (Select a) = a

  encode = Selected

  decode _ (Selected x) = return x
  decode sol (Choice b x y) =
    do b' <- decode sol b
       decode sol (if b' then y else x)

runSelectWith :: FromBit b => (a -> b) -> Select a -> b
runSelectWith f x = runSelect (fmap f x)

instance Functor Select where
  fmap = liftM

instance Applicative Select where
  pure = Selected
  (<*>) = ap

instance Monad Select where
  Selected x   >>= f = f x
  Choice b x y >>= f = Choice b (x >>= f) (y >>= f)

newtype SelectBuilder s f a = SB { runSelectBuilder :: f (Select a) }

instance (HasSAT s, MonadState s m) => Semigroup (SelectBuilder s m a) where
  SB l <> SB r = SB (liftA3 Choice exists l r)

singleton :: Applicative m => a -> Option (SelectBuilder s m a)
singleton = Option . Just . SB . pure . Selected

foldSelect :: (HasSAT s, MonadState s m, Foldable t) => t a -> Maybe (m (Select a))
foldSelect t = runSelectBuilder <$> getOption (foldMap singleton t)

selectEq :: Eq a => Select a -> Select a -> Bit
selectEq xs ys = runSelect (liftA2 (\x y -> bool (x == y)) xs ys)

instance Equatable a => Equatable (Select a) where
  x === y = runSelect (liftA2 (===) x y)
