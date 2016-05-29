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


regexp :: { RegExpFull Char }
  : regexp '|' regexp01 { $1 ||| $3 }
  | regexp01            { $1        }

regexp01
  : regexp1 { $1    }
  |         { empty }

regexp1
  : regexp1 aregexp     { $1 >>> $2 }
  | aregexp             { $1        }

aregexp
  : '(' regexp ')'        { grouping $2             }
  | aregexp '*'           { rep $1                  }
  | aregexp '+'           { $1 >>> rep $1           }
  | aregexp '?'           { $1 ||| empty            }
  | '\\' DIGIT            { backref (digitToInt $2) }
  | ALPHA                 { one $1                  }
  | DIGIT                 { one $1                  }
  | '.'                   { anyone                  }
  | '[' letterset ']'     { oneOf $2                }
  | '[' '^' letterset ']' { noneOf $3               }

letterset :: { String }
  : ALPHA           { [ $1 ]  }
  | letterset ALPHA { $2 : $1 }

{
check :: (a -> Bool) -> a -> Maybe a
check p x
  | p x = Just x
  | otherwise = Nothing
}
