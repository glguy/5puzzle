{-# Language TemplateHaskell #-}
module RegExp.BackMatch where

import RegExp.AST
import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad
import Control.Lens hiding (Empty)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding ((&&),(||),all,and,any,or,not)
import Ersatz

data ParserState a = ParserState
  { _tokens :: [a]
  , _position :: !Int
  , _nextGroupId :: !Int
  , _backgroups :: Map Int [a]
  }

makeLenses ''ParserState

type M a = StateT (ParserState a) []

backMatch :: Equatable a => RegExpFull a -> [a] -> Bit
backMatch regexp inp = or (evalStateT body st0)
  where
  body = do res <- foldRegExpFull process regexp
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

  process (RegF r) = process' r

  process' Empty       = return true
  process' (Seq _ x y) = liftA2 (&&) x y
  process' (Alt _ x y) = x <|> y
  process' (Rep m)     = and <$> many (nonempty m)

  process' (OneOf mode xs) =
    do x <- next
       let match = any (x ===) xs
       case mode of
         InSet -> return match
         NotInSet -> return (not match)

nonempty :: M t a -> M t a
nonempty m =
  do start <- use position
     res <- m
     end   <- use position
     guard (end > start)
     return res

withMatched :: M t a -> M t (a, [t])
withMatched x =
  do pos  <- use position
     toks <- use tokens
     res  <- x
     pos' <- use position
     let used = take (pos' - pos) toks
     return (res, used)

nextN :: Int -> M t [t]
nextN n =
  do toks <- tokens %%= splitAt n
     position += n
     guard (n == length toks)
     return toks

next :: M t t
next =
  do x:xs <- use tokens
     tokens .= xs
     position += 1
     return x
