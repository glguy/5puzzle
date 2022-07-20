{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- Based on "A Play on Regular RegExpessions"
by Sebastian Fischer, Franks Huch, and Thomas Wilke
-}
module RegExp.Match  where

import RegExp.AST
import Prelude hiding ((||),(&&),not,any)
import Ersatz (Boolean(..))

data RegS b a = RegS { matched :: b, expr :: RegF Bool a (RegS b a) }

instance Functor (RegS b) where
  fmap f r = r { expr = mapRegF f (expr r) }

instance AcceptsEmpty (RegS b a) where
  acceptsEmpty = acceptsEmpty . expr

advanceRegS :: Boolean b => (a -> b) -> RegS b a -> b -> RegS b a
advanceRegS p = update
  where
   update re isFirst =
    case expr re of
        Empty -> RegS { matched = false, expr = Empty }

        OneOf mode cs ->
             let stb = any p cs
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

match :: Boolean c => (a -> b -> c) -> RegExp b -> [a] -> c
match _  e [] = bool (acceptsEmpty e)
match eq e (c : cs) = matched s2
  where
  s0 = foldRegExp (RegS false) e
  s1 = advanceRegS (eq c) s0 true
  s2 = foldl (\s c1 -> advanceRegS (eq c1) s false) s1 cs
