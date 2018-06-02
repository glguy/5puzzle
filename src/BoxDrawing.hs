module BoxDrawing
  ( Weight(Thin, Heavy, Double)
  , Orient(Horiz, Vert)
  , boxChar
  , renderGrid
  ) where

import Coord
import Data.Maybe (fromMaybe)
import Data.List (transpose)
import Control.Monad (guard)

data Weight = Thin | Heavy | Double
  deriving (Eq, Ord, Read, Show)

data Orient = Horiz | Vert
  deriving (Eq, Ord, Read, Show)

boxChar ::
  Maybe Weight {- ^ up                    -} ->
  Maybe Weight {- ^ down                  -} ->
  Maybe Weight {- ^ left                  -} ->
  Maybe Weight {- ^ right                 -} ->
  Maybe Char   {- ^ box drawing character -}
boxChar Nothing       Nothing       Nothing       Nothing       = Just ' '
boxChar (Just Thin  ) Nothing       Nothing       Nothing       = Just '╵'
boxChar (Just Heavy ) Nothing       Nothing       Nothing       = Just '╹'
boxChar Nothing       (Just Thin  ) Nothing       Nothing       = Just '╷'
boxChar Nothing       (Just Heavy ) Nothing       Nothing       = Just '╻'
boxChar Nothing       Nothing       (Just Thin  ) Nothing       = Just '╴'
boxChar Nothing       Nothing       (Just Heavy ) Nothing       = Just '╸'
boxChar Nothing       Nothing       Nothing       (Just Thin  ) = Just '╶'
boxChar Nothing       Nothing       Nothing       (Just Heavy ) = Just '╺'
boxChar (Just Thin  ) (Just Thin  ) Nothing       Nothing       = Just '│'
boxChar (Just Heavy ) (Just Heavy ) Nothing       Nothing       = Just '┃'
boxChar (Just Double) (Just Double) Nothing       Nothing       = Just '║'
boxChar (Just Thin  ) (Just Heavy ) Nothing       Nothing       = Just '╽'
boxChar (Just Heavy ) (Just Thin  ) Nothing       Nothing       = Just '╿'
boxChar (Just Thin  ) Nothing       (Just Thin  ) Nothing       = Just '┘'
boxChar (Just Heavy ) Nothing       (Just Heavy ) Nothing       = Just '┛'
boxChar (Just Double) Nothing       (Just Double) Nothing       = Just '╝'
boxChar (Just Thin  ) Nothing       (Just Heavy ) Nothing       = Just '┙'
boxChar (Just Heavy ) Nothing       (Just Thin  ) Nothing       = Just '┚'
boxChar (Just Thin  ) Nothing       (Just Double) Nothing       = Just '╛'
boxChar (Just Double) Nothing       (Just Thin  ) Nothing       = Just '╜'
boxChar (Just Thin  ) Nothing       Nothing       (Just Thin  ) = Just '└'
boxChar (Just Heavy ) Nothing       Nothing       (Just Heavy ) = Just '┗'
boxChar (Just Double) Nothing       Nothing       (Just Double) = Just '╚'
boxChar (Just Thin  ) Nothing       Nothing       (Just Heavy ) = Just '┕'
boxChar (Just Heavy ) Nothing       Nothing       (Just Thin  ) = Just '┖'
boxChar (Just Thin  ) Nothing       Nothing       (Just Double) = Just '╘'
boxChar (Just Double) Nothing       Nothing       (Just Thin  ) = Just '╙'
boxChar Nothing       (Just Thin  ) (Just Thin  ) Nothing       = Just '┐'
boxChar Nothing       (Just Heavy ) (Just Heavy ) Nothing       = Just '┓'
boxChar Nothing       (Just Double) (Just Double) Nothing       = Just '╗'
boxChar Nothing       (Just Thin  ) (Just Heavy ) Nothing       = Just '┑'
boxChar Nothing       (Just Heavy ) (Just Thin  ) Nothing       = Just '┒'
boxChar Nothing       (Just Thin  ) (Just Double) Nothing       = Just '╕'
boxChar Nothing       (Just Double) (Just Thin  ) Nothing       = Just '╖'
boxChar Nothing       (Just Thin  ) Nothing       (Just Thin  ) = Just '┌'
boxChar Nothing       (Just Heavy ) Nothing       (Just Heavy ) = Just '┏'
boxChar Nothing       (Just Double) Nothing       (Just Double) = Just '╔'
boxChar Nothing       (Just Thin  ) Nothing       (Just Heavy ) = Just '┍'
boxChar Nothing       (Just Heavy ) Nothing       (Just Thin  ) = Just '┎'
boxChar Nothing       (Just Thin  ) Nothing       (Just Double) = Just '╒'
boxChar Nothing       (Just Double) Nothing       (Just Thin  ) = Just '╓'
boxChar Nothing       Nothing       (Just Thin  ) (Just Thin  ) = Just '─'
boxChar Nothing       Nothing       (Just Heavy ) (Just Heavy ) = Just '━'
boxChar Nothing       Nothing       (Just Thin  ) (Just Heavy ) = Just '╼'
boxChar Nothing       Nothing       (Just Heavy ) (Just Thin  ) = Just '╾'
boxChar Nothing       Nothing       (Just Double) (Just Double) = Just '═'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) Nothing       = Just '┤'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) Nothing       = Just '┫'
boxChar (Just Double) (Just Double) (Just Double) Nothing       = Just '╣'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) Nothing       = Just '┦'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) Nothing       = Just '┧'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) Nothing       = Just '┥'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) Nothing       = Just '┪'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) Nothing       = Just '┩'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) Nothing       = Just '┨'
boxChar (Just Thin  ) (Just Thin  ) (Just Double) Nothing       = Just '╡'
boxChar (Just Double) (Just Double) (Just Thin  ) Nothing       = Just '╢'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Thin  ) = Just '├'
boxChar (Just Heavy ) (Just Heavy ) Nothing       (Just Heavy ) = Just '┣'
boxChar (Just Double) (Just Double) Nothing       (Just Double) = Just '╠'
boxChar (Just Heavy ) (Just Thin  ) Nothing       (Just Thin  ) = Just '┞'
boxChar (Just Thin  ) (Just Heavy ) Nothing       (Just Thin  ) = Just '┟'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Heavy ) = Just '┝'
boxChar (Just Thin  ) (Just Heavy ) Nothing       (Just Heavy ) = Just '┢'
boxChar (Just Heavy ) (Just Thin  ) Nothing       (Just Heavy ) = Just '┡'
boxChar (Just Heavy ) (Just Heavy ) Nothing       (Just Thin  ) = Just '┠'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Double) = Just '╞'
boxChar (Just Double) (Just Double) Nothing       (Just Thin  ) = Just '╟'
boxChar (Just Thin  ) Nothing       (Just Thin  ) (Just Thin  ) = Just '┴'
boxChar (Just Heavy ) Nothing       (Just Heavy ) (Just Heavy ) = Just '┻'
boxChar (Just Double) Nothing       (Just Double) (Just Double) = Just '╩'
boxChar (Just Heavy ) Nothing       (Just Thin  ) (Just Thin  ) = Just '┸'
boxChar (Just Thin  ) Nothing       (Just Heavy ) (Just Thin  ) = Just '┵'
boxChar (Just Thin  ) Nothing       (Just Thin  ) (Just Heavy ) = Just '┶'
boxChar (Just Thin  ) Nothing       (Just Heavy ) (Just Heavy ) = Just '┷'
boxChar (Just Heavy ) Nothing       (Just Thin  ) (Just Heavy ) = Just '┺'
boxChar (Just Heavy ) Nothing       (Just Heavy ) (Just Thin  ) = Just '┹'
boxChar (Just Thin  ) Nothing       (Just Double) (Just Double) = Just '╧'
boxChar (Just Double) Nothing       (Just Thin  ) (Just Thin  ) = Just '╨'
boxChar Nothing       (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '┬'
boxChar Nothing       (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '┳'
boxChar Nothing       (Just Double) (Just Double) (Just Double) = Just '╦'
boxChar Nothing       (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '┰'
boxChar Nothing       (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '┭'
boxChar Nothing       (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '┮'
boxChar Nothing       (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '┯'
boxChar Nothing       (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '┲'
boxChar Nothing       (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '┱'
boxChar Nothing       (Just Thin  ) (Just Double) (Just Double) = Just '╤'
boxChar Nothing       (Just Double) (Just Thin  ) (Just Thin  ) = Just '╥'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '┼'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '╀'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '╁'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '┽'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '┾'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '╂'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '╃'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '╄'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '╅'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '╆'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '┿'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '╈'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '╇'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '╊'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '╉'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '╋'
boxChar (Just Double) (Just Double) (Just Double) (Just Double) = Just '╬'
boxChar (Just Thin  ) (Just Thin  ) (Just Double) (Just Double) = Just '╪'
boxChar (Just Double) (Just Double) (Just Thin  ) (Just Thin  ) = Just '╫'
boxChar _             _             _             _             = Nothing


renderGrid ::
  Int                               {- ^ width      -} ->
  Int                               {- ^ height     -} ->
  (Coord -> Orient -> Maybe Weight) {- ^ edge logic -} ->
  (Coord -> Char)                   {- ^ cell logic -} ->
  String
renderGrid w h edge cell = rearrange [[render1 (C x y) | x <- [0..w]] | y <- [0..h]]
  where
    boxChar' u d l r = fromMaybe ' ' (boxChar u d l r)

    -- rows of cells of (lines in cell) to single string
    rearrange :: [[[String]]] -> String
    rearrange = unlines . map concat . concat . map transpose

    render1 c@(C x y) = line1:[line2 | y<h]
      where
        eR = guard (x<w) >> edge c        Horiz
        eD = guard (y<h) >> edge c        Vert
        eL = guard (0<x) >> edge (left c) Horiz
        eU = guard (0<y) >> edge (up   c) Vert

        line1 = boxChar' eU eD eL eR
              : [boxChar' Nothing Nothing eR eR | x<w]
        line2 = boxChar' eD eD Nothing Nothing
              : [cell c | x<w]
