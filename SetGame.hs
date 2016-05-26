{-# Language TypeFamilies #-}
{-# Language DeriveGeneric #-}
module Main where

import Ersatz
import Booleans
import Control.Monad.State
import Control.Applicative
import Data.List (tails)
import GHC.Generics
import Prelude hiding (not, all, and, (&&), (||))

data Color = Red | Green | Blue          deriving (Eq, Show, Enum)
data Shape = Oval | Squiggle | Chevron   deriving (Eq, Show, Enum)
data Count = One | Two | Three           deriving (Eq, Show, Enum)
data Fill  = Solid | Hollow | Striped    deriving (Eq, Show, Enum)

data Card = Card Color Shape Count Fill deriving (Show)

------------------------------------------------------------------------

data SCard = SCard { scardColor, scardShape, scardCount, scardFill :: Fin3 }
  deriving Generic

instance Variable SCard
instance Equatable SCard
instance Codec SCard where
  type Decoded SCard = Card
  encode (Card a b c d) = SCard (aux a) (aux b) (aux c) (aux d)
        where
        aux :: Enum a => a -> Fin3
        aux x = encode (fromEnum x)
  decode sol (SCard a b c d) =
    do a' <- decode sol a
       b' <- decode sol b
       c' <- decode sol c
       d' <- decode sol d
       return (Card (toEnum a') (toEnum b') (toEnum c') (toEnum d'))

------------------------------------------------------------------------

newtype Fin3 = Fin3 Bit2
  deriving Generic

instance Variable Fin3 where
  literally m =
    do Bit2 a b <- literally m
       return (Fin3 (Bit2 a (not a && b)))

instance Equatable Fin3

instance Codec Fin3 where
  type Decoded Fin3 = Int
  encode 0 = Fin3 (encode 0)
  encode 1 = Fin3 (encode 1)
  encode 2 = Fin3 (encode 2)
  encode _ = error "Fin3.encode: out of range"

  decode sol (Fin3 a) = fromIntegral <$> decode sol a

------------------------------------------------------------------------

match3 :: Equatable a => a -> a -> a -> Bit
match3 x y z = same || different
  where
  same      = x === y && y === z && x === z
  different = x /== y && y /== z && x /== z

uniques :: Equatable a => [a] -> Bit
uniques xs = nor
           $ do y:ys <- tails xs
                z    <- ys
                return (y === z)

goodSet :: (SCard, SCard, SCard) -> Bit
goodSet (c1,c2,c3) =
  and [aux scardColor, aux scardShape, aux scardCount, aux scardFill]
  where
  aux sel = match3 (sel c1) (sel c2) (sel c3)

problem :: (HasSAT s, MonadState s m) => m [(SCard, SCard, SCard)]
problem =
  do cards <- replicateM 81 exists
     assert (uniques cards)

     let sets = threes cards
     assert (all goodSet sets)

     return sets

main :: IO ()
main =
  do (Satisfied, Just sets) <- solveWith minisat problem
     mapM_ print sets

threes :: [a] -> [(a,a,a)]
threes (x:y:z:rest) = (x,y,z) : threes rest
threes []           = []
threes _            = error "threes: Expected length to be multiple of three"
