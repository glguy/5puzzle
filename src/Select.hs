{-# Language TypeFamilies #-}
module Select
  ( Select
  , runSelect
  , runSelectWith
  , select
  ) where

import Ersatz
import FromBit
import Data.Maybe
import Data.List (findIndex)
import Control.Monad.State
import Prelude hiding (and, or, (&&), (||) ,not)

-- | A set of choices and an index of the chosen element of that set
data Select a = Selected a | Choice Bit (Select a) (Select a)

select :: (MonadState s m, HasSAT s) => [a] -> m (Select a)
select xs = merge (map Selected xs)
  where
  merge []  = error "select: empty list"
  merge [x] = return x
  merge xs  = merge =<< reduce xs =<< exists

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
