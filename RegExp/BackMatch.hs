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

backMatch :: RegExp (Either Int Bit8) -> [Bit8] -> Bit
backMatch re inp = or (evalStateT body st0)
  where
  body = do res <- foldRegExp aux re
            []  <- use tokens
            return res

  st0 = ParserState { _tokens = inp
                    , _position = 0
                    , _nextGroupId = 0
                    , _backgroups = Map.empty
                    }

  aux Empty = return true
  aux (Rep m) = and <$> many m
  aux (OneOf _ [Left d]) = -- hack to identify backrefs
   do Just g <- use (backgroups . at d)
      toks   <- tokens %%= splitAt (length g)
      position += length g
      guard (length g <= length toks)
      return (toks === g)
  aux (OneOf mode xs) =
    do x <- next
       let match = any (x ===) (map fromRight xs)
       case mode of
         InSet -> return match
         NotInSet -> return (not match)
  aux (Seq _ x y) = liftA2 (&&) x y
  aux (Alt _ x y) = x <|> y
  aux (Group _ x) =
    do gid <- nextGroupId <+= 1

       pos  <- use position
       toks <- use tokens
       res  <- x
       pos' <- use position

       backgroups . at gid ?= take (pos' - pos) toks

       return res

fromRight (Right x) = x

next :: M Bit8
next =
  do x:xs <- use tokens
     tokens .= xs
     position += 1
     return x
