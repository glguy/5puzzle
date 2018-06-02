{-

My wife and I recently attended a party at which there were four other married
couples. Various handshakes took place. No one shook hands with oneself, nor
with one's spouse, and no one shook hands with the same person more than once.
After all the handshakes were over, I asked each person, including my wife, how
many hands he (or she) had shaken. To my surprise each gave a different answer.
How many hands did my wife shake?

http://http://www.cut-the-knot.org/pigeonhole/FiveCouples.shtml

-}

module Main where

import Control.Applicative
import Booleans
import Ersatz
import           Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding ((&&), (||), all)

data Spouse = Husband | Wife deriving (Eq, Ord, Show, Read)

couples :: Int
couples = 5

type Handshakes = Map (Int, Spouse, Int, Spouse) Bit

countHandshakes :: Handshakes -> Int -> Spouse -> Bits
countHandshakes m x xS = countBits [ met | ((i,iS,j,jS),met) <- Map.toList m
                                         , i == x && iS == xS ||
                                           j == x && jS == xS
                                         ]

handshakesExist :: MonadSAT s m => m (Map (Int, Spouse, Int, Spouse) Bit)
handshakesExist
  = sequence
  $ Map.fromList [ ((i,iS,j,jS), exists)
                 | i  <- [1..couples]
                 , iS <- [Husband,Wife]
                 , j  <- [i+1..couples]
                 , jS <- [Husband,Wife]
                 ]

problem :: MonadSAT s m  => m Bits
problem =
  do m <- handshakesExist
     let consider = (1,Wife) : liftA2 (,) [2..couples] [Husband,Wife]
         handshakes = uncurry (countHandshakes m) <$> consider
     assert (unique handshakes)
     return (head handshakes)

main :: IO ()
main =
  do Just res <- getModel problem
     Nothing  <- getModel (problem `checking` (/== encode res))
     print res

