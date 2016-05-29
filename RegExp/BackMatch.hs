{-# Language TemplateHaskell #-}
module RegExp.BackMatch where

import RegExp.AST
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad
import Control.Lens hiding (Empty)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding ((&&),(||),all,and,any,or,not)
import Ersatz

data ParserState = ParserState
  { _tokens :: [Bit8]
  , _position :: !Int
  , _nextGroupId :: !Int
  , _backgroups :: Map Int [Bit8]
  }

makeLenses ''ParserState

type M = StateT ParserState []

backMatch :: RegExpFull Bit8 -> [Bit8] -> Bit
backMatch re inp = or (evalStateT body st0)
  where
  body = do res <- foldRegExpFull process re
            []  <- use tokens -- consume full input
            return res

  st0 = ParserState { _tokens      = inp
                    , _position    = 0
                    , _nextGroupId = 0
                    , _backgroups  = Map.empty
                    }

  process (Group x) =
    do gid      <- nextGroupId <+= 1
       (res, g) <- withMatched x
       backgroups . at gid ?= g
       return res

  process (BackRef d) =
   do Just g <- use (backgroups . at d)
      toks   <- nextN (length g)
      return (toks === g)

  process (RegF r) = simple r

  simple Empty       = return true
  simple (Seq _ x y) = liftA2 (&&) x y
  simple (Alt _ x y) = x <|> y
  simple (Rep m)     = and <$> many m

  simple (OneOf mode xs) =
    do x <- next
       let match = any (x ===) xs
       case mode of
         InSet -> return match
         NotInSet -> return (not match)

withMatched :: M a -> M (a, [Bit8])
withMatched x =
  do pos  <- use position
     toks <- use tokens
     res  <- x
     pos' <- use position
     let used = take (pos' - pos) toks
     return (res, used)

nextN :: Int -> M [Bit8]
nextN n =
  do toks <- tokens %%= splitAt n
     position += n
     guard (n == length toks)
     return toks

next :: M Bit8
next =
  do x:xs <- use tokens
     tokens .= xs
     position += 1
     return x
