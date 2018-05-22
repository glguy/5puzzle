{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language DefaultSignatures #-}
module ChooseBit where

import Prelude hiding ((&&), not)
import Ersatz
import GHC.Generics

-- | Class for values that can be symbolically selected.
class ChooseBit a where
  chooseBit ::
    a   {- ^ false case -} ->
    a   {- ^ true  case -} ->
    Bit {- ^ selector   -} ->
    a
  default chooseBit :: (Generic a, GChooseBit (Rep a)) => a -> a -> Bit -> a
  chooseBit x y b = to (gchooseBit (from x) (from y) b)

instance ChooseBit Bit where chooseBit = choose
instance ChooseBit Bit1
instance ChooseBit Bit2
instance ChooseBit Bit3
instance ChooseBit Bit4
instance ChooseBit Bit5
instance ChooseBit Bit6
instance ChooseBit Bit7
instance ChooseBit Bit8

instance ChooseBit Bits where
  chooseBit (Bits x0) (Bits y0) b = Bits (merge x0 y0)
    where
      merge []     ys     = map (    b &&) ys
      merge xs     []     = map (not b &&) xs
      merge (x:xs) (y:ys) = choose x y b : merge xs ys

class GChooseBit f where
  gchooseBit :: f a -> f a -> Bit -> f a

instance GChooseBit f => GChooseBit (M1 i c f) where
  gchooseBit (M1 x) (M1 y) b = M1 (gchooseBit x y b)

instance (GChooseBit f, GChooseBit g) => GChooseBit (f :*: g) where
  gchooseBit (x1 :*: x2) (y1 :*: y2) b = gchooseBit x1 y1 b :*: gchooseBit x2 y2 b

instance ChooseBit a => GChooseBit (K1 i a) where
  gchooseBit (K1 x) (K1 y) b = K1 (chooseBit x y b)
