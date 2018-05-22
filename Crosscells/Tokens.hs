module Crosscells.Tokens
  ( Direction(..)
  , Token(..)
  , parseTokens
  ) where

import Data.Char
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as V

import Crosscells.Region

data Token
  = Arrow Direction
  | Box
  | Plus Int
  | Times Int
  | Bracketed Int
  | Number Int
  deriving (Read, Show, Eq, Ord)

parseTokens :: String -> [(Region, Token)]
parseTokens = parseRaw . V.fromList . map V.fromList . lines

isStart :: Vector Char -> Int -> Bool
isStart row i = 0==i || (row V.! (i-1)) `elem` "] |"

isBoxStart :: Vector (Vector Char) -> Vector Char -> Char -> Int -> Int -> Bool
isBoxStart file row col rowIx colIx =
  col == '+' && colR == Just '-' && colD == Just '|'
  where
    colR = row V.!? (colIx+1)
    colD = do row1 <- file V.!? (rowIx+1)
              row1 V.!? colIx



parseRaw :: Vector (Vector Char) -> [(Region, Token)]
parseRaw file =
  [ result
    | (rowIx, row) <- toList (V.indexed file)
    , (colIx, col) <- toList (V.indexed row)
    , isStart row colIx
    , Just result <- [parseRaw1 file row col rowIx colIx]
    ]

parseRaw1 :: Vector (Vector Char) -> Vector Char -> Char -> Int -> Int -> Maybe (Region, Token)
parseRaw1 file row col rowIx colIx

  | isBoxStart file row col rowIx colIx = Just (parseBox file rowIx colIx, Box)

  | '+' == col, maybe False isDigit (row V.!? (colIx+1)) =
       let (val, width) = parseNumber (V.drop (colIx+1) row)
       in  Just (Region (Coord rowIx colIx) (Coord rowIx (colIx + width)), Plus val)

  | 'x' == col =
       let (val, width) = parseNumber (V.drop (colIx+1) row)
       in  Just (Region (Coord rowIx colIx) (Coord rowIx (colIx + width)), Times val)

  | '[' == col =
       let (val, width) = parseNumber (V.drop (colIx+1) row)
       in  Just (Region (Coord rowIx colIx) (Coord rowIx (colIx + width + 1)), Bracketed val)

  | '^' == col = Just (Region (Coord rowIx colIx) (Coord rowIx colIx), Arrow U)
  | 'v' == col = Just (Region (Coord rowIx colIx) (Coord rowIx colIx), Arrow D)
  | '<' == col = Just (Region (Coord rowIx colIx) (Coord rowIx colIx), Arrow L)
  | '>' == col = Just (Region (Coord rowIx colIx) (Coord rowIx colIx), Arrow R)

  | isDigit col =
       let (val, width) = parseNumber (V.drop colIx row)
       in  Just (Region (Coord rowIx colIx) (Coord rowIx (colIx + width - 1)), Number val)

  | otherwise = Nothing

parseNumber :: Vector Char -> (Int, Int)
parseNumber v = (read (toList fragment), V.length fragment)
  where
    fragment = V.takeWhile isDigit v

parseBox :: Vector (Vector Char) -> Int -> Int -> Region
parseBox file rowIx colIx = Region (Coord rowIx colIx) (Coord rowIx' colIx')
  where
    Just width = V.elemIndex '+' (V.drop (colIx+1) (file V.! rowIx))
    colIx' = colIx + 1 + width
    Just height = V.findIndex (\row -> row V.!? colIx' == Just '+')
                              (V.drop (rowIx+1) file)
    rowIx' = rowIx + 1 + height
