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
import Prelude hiding (or, (&&))

-- | A set of choices and an index of the chosen element of that set
data Select a = Selected a | Choice Bit (Select a) (Select a)

select :: (MonadState s m, HasSAT s) => [a] -> m (Select a)
select xs = merge (map Selected xs)
  where
  merge []  = error "select: empty list"
  merge [x] = return x
  merge xs  = do b   <- exists
                 xs' <- reduce b xs
                 merge xs'

  reduce b (x1:x2:xs) =
     do xs' <- reduce b xs
        return (Choice b x1 x2 : xs')
  reduce _ xs = return xs

runSelect :: FromBit a => Select a -> a
runSelect (Selected x)   = x
runSelect (Choice b x y) = choose (runSelect x) (runSelect y) (fromBit b)

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
