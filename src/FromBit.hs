{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language DefaultSignatures #-}
module FromBit where

import Ersatz
import GHC.Generics

-- | Class for 'Boolean' instances that can select 'true' and 'false'
-- using a 'Bit'
--
-- This can't be a member of 'Boolean' because 'Bool' is an instance.
--
-- @fromBit true  === true@
-- @fromBit false === false@
--
-- The default implementation for product types works point-wise
-- on each field using 'fromBit'.
class Boolean a => FromBit a where
  fromBit :: Bit -> a
  default fromBit :: (Generic a, GFromBit (Rep a)) => Bit -> a
  fromBit = to . gfromBit

instance FromBit Bit where fromBit = id
instance FromBit Bit1
instance FromBit Bit2
instance FromBit Bit3
instance FromBit Bit4
instance FromBit Bit5
instance FromBit Bit6
instance FromBit Bit7
instance FromBit Bit8

class GFromBit f where
  gfromBit :: Bit -> f a

instance GFromBit f => GFromBit (M1 i c f) where
  gfromBit = M1 . gfromBit

instance (GFromBit f, GFromBit g) => GFromBit (f :*: g) where
  gfromBit x = gfromBit x :*: gfromBit x

instance FromBit a => GFromBit (K1 i a) where
  gfromBit = K1 . fromBit
