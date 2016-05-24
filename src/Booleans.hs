module Booleans (isTrue, exactlyOne) where

import Ersatz
import Data.List (mapAccumL)
import Prelude hiding ((&&), (||))

-- | Returns a summary value of where a boolean is true in exactly
-- one position in the list.
exactlyOne :: Boolean a => [a] -> a
exactlyOne xs = allCovered && nor overlaps
  where
  (allCovered, overlaps) = mapAccumL addMask false xs

  addMask covered mask = (covered || mask, covered && mask)

isTrue :: (Equatable a, Boolean a) => a -> Bit
isTrue x = true === x
