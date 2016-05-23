{-# Language TypeFamilies #-}
module Select where

import Ersatz
import Data.Maybe
import Data.List (findIndex)
import Control.Monad.State
import Prelude hiding (or)

-- | A set of choices and an index of the chosen element of that set
data Select a = Select [a] Bits

select :: (MonadState s m, HasSAT s) => [a] -> m (Select a)
select xs = Select xs <$> existsNat (length xs)

foldSelect :: Boolean b => (Bit -> a -> b) -> Select a -> b
foldSelect f (Select xs i) = or [ f (i === fromIntegral j) x | (j,x) <- zip [0 :: Int ..] xs ]

instance Codec (Select a) where

  type Decoded (Select a) = a

  encode x = Select [x] 0

  decode sol (Select xs i) =
    do j <- decode sol i
       return (xs !! fromIntegral j)

------------------------------------------------------------------------
-- Symbolic variables with dynamic range

-- | Bits needed to distinguish the given number of elements
bitsNeeded :: Int -> Int
bitsNeeded x = fromJust (findIndex (>= x) (iterate (*2) 1))

-- | Generate a variable-bit symbolic term capable of representing the
-- natural numbers below the given bound.
existsNat :: (MonadState s m, HasSAT s) => Int -> m Bits
existsNat bound =
  do x <- Bits <$> replicateM (bitsNeeded bound) exists
     assert (x <? fromIntegral bound)
     return x
