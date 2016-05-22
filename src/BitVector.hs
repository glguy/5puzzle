module BitVector (BitVector, index, fromList, constant) where

import Ersatz
import Prelude hiding (and, (&&), (||), not)

-- | Infinitely defined bit vectors represented as a list of
-- individual values along with a default value for the remaining
-- values.
data BitVector = BitVector [Bit] Bit

fromList :: [Bit] -> BitVector
fromList xs = BitVector xs false

constant :: Bit -> BitVector
constant = BitVector []

instance Boolean BitVector where
  bool = constant . bool
  (&&) = op2 (&&)
  (||) = op2 (||)
  xor  = op2 xor
  not (BitVector xs x) = BitVector (not <$> xs) (not x)
  all f    = foldr (\y ys -> f y && ys) true
  any f    = foldr (\y ys -> f y || ys) false

instance Equatable BitVector where
  x === y = andBitVector (op2 (===) x y)

op2 :: (Bit -> Bit -> Bit) -> BitVector -> BitVector -> BitVector
op2 (?) (BitVector xs xdef) (BitVector ys ydef) = BitVector (go xs ys) (xdef ? ydef)
  where
  go (x:xs) (y:ys) = x?y : go xs ys
  go []     ys     = map (xdef?) ys
  go xs     []     = map (?ydef) xs

andBitVector :: BitVector -> Bit
andBitVector (BitVector xs x) = and (x:xs)

index :: BitVector -> Int -> Bit
index (BitVector xs x) i =
  case drop i xs of
    []  -> x
    y:_ -> y
