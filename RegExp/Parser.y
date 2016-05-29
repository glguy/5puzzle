{

{-# Language ViewPatterns #-}
module RegExp.Parser where

import RegExp.AST
import Data.Char

}

%tokentype { Char }
%token

'[' { '[' }
']' { ']' }
'(' { '(' }
')' { ')' }
'.' { '.' }
'\\' { '\\' }
'?' { '?' }
'+' { '+' }
'^' { '^' }
'|' { '|' }
'*' { '*' }
DIGIT { (check isDigit -> Just (??)) }
ALPHA { (check isAlpha -> Just (??)) }

%monad { Either String }
%error { Left }

%name parseRegExp regexp

%%

regexp :: { RegExp Char }
  : alts { $1    }
  |      { empty }

alts : alts '|' seqs { $1 ||| $3 }
     | seqs          { $1 }

seqs : seqs aregexp { $1 >>> $2 }
     |              { empty }

aregexp :: { RegExp Char }
  : '(' regexp ')' { grouping $2 }
  | aregexp '*'    { rep $1 }
  | aregexp '+'    { $1 >>> rep $1 }
  | aregexp '?'    { $1 ||| empty }
  | '\\' DIGIT     { one $2 }
  | ALPHA          { one $1 }
  | '.'            { anyone }
  | '[' letterset ']' { oneOf $2 }
  | '[' '^' letterset ']' { noneOf $3 }

letterset :: { String }
  : ALPHA           { [ $1 ]  }
  | letterset ALPHA { $2 : $1 }

{
check :: (a -> Bool) -> a -> Maybe a
check p x
  | p x = Just x
  | otherwise = Nothing
}
