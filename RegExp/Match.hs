{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- Based on "A Play on Regular RegExpessions"
by Sebastian Fischer, Franks Huch, and Thomas Wilke
-}
module RegExp.Match  where

import RegExp.AST
import Prelude hiding ((||),(&&),not,any)

import Ersatz

data RegS a = RegS { matched :: Bit, expr :: RegF Bool a (RegS a) }

instance AcceptsEmpty (RegS a) where
        acceptsEmpty e = acceptsEmpty (expr e)

advanceRegS :: Equatable a => a -> RegS a -> Bit -> RegS a
advanceRegS c = update
  where
   update re isFirst =
    case expr re of
        Empty -> do
           RegS { matched = false, expr = Empty }

        OneOf mode cs ->
             let stb = any (c ===) cs
                 stb' = case mode of
                          InSet    -> stb
                          NotInSet -> not stb
                 mat = isFirst && stb'
             in RegS { matched = mat, expr = OneOf mode cs }

        Alt b e1 e2 ->
             let s1  = update e1 isFirst
                 s2  = update e2 isFirst
                 mat = matched s1 || matched s2
             in RegS { matched = mat, expr = Alt b s1 s2}

        Seq b e1 e2 ->
             let s1 = update e1 isFirst
                 isFirst'
                   | acceptsEmpty e1 = matched e1 || isFirst
                   | otherwise       = matched e1
                 s2 = update e2 isFirst'
                 mat | acceptsEmpty e2 = matched s2 || matched s1
                     | otherwise       = matched s2
             in RegS { matched = mat, expr = Seq b s1 s2 }

        Rep e ->
             let isFirst' = isFirst || matched e
                 s = update e isFirst'
             in RegS { matched = matched s, expr = Rep s }

match :: Equatable a => RegExp a -> [a] -> Bit
match e [] = bool (acceptsEmpty e)
match e (c : cs) = matched s2
  where
  s0 = foldRegExp (RegS false) e
  s1 = advanceRegS c s0 true
  s2 = foldl (\s c1 -> advanceRegS c1 s false) s1 cs
