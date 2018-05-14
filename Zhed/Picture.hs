module Zhed.Picture where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import qualified Data.Map as Map

import Zhed.Puzzle

cellSize = 50 :: Double

renderSolutionSVG :: Puzzle -> [(Coord, Int, Dir)] -> FilePath -> IO ()
renderSolutionSVG puzzle solution path = renderSVG path sz diagram

  where
  Coord xmax ymax = puzzleBounds puzzle
  sz = mkSizeSpec (V2 Nothing Nothing)

  used = [ c | (c,_,_) <- solution ]

  subdiagrams
     = Map.fromList
     $ [ (c, plainSquare blue) | c <- puzzleTarget puzzle ]
    ++ [ (c, plainSquare green) | (c,_) <- puzzleSquares puzzle, c `notElem` used]
    ++ [ (c,  numLabel i <> pointedSquare dir) | (i,(c,_n,dir)) <- zip [1::Int ..] solution ]

  numLabel :: Int -> Diagram SVG
  numLabel i = fontSize (normalized 0.04) $ fc black $ text $ show i

  diagram :: Diagram SVG
  diagram =
    mconcat [ translate (V2 ( fromIntegral x * cellSize)
                            (-fromIntegral y * cellSize))
                        (Map.findWithDefault (plainSquare lightgray # lc lightgray) (Coord x y) subdiagrams)
            | x <- [0..xmax], y <- [0..ymax] ]

plainSquare :: Colour Double -> Diagram SVG
plainSquare c
  = fc c
  $ square (40 :: Double)

pointedSquare :: Dir -> Diagram SVG
pointedSquare dir
  = rotateBy (rot/4)
  $ fc yellow
  $ strokeLocLoop
  $ fromVertices
  $ map (P . r2) [ (-20,-20), (0,-25), (20,-20), (20,20), (-20,20), (-20,-20) ]
  where
    rot = case dir of
            D -> 0
            R -> 1
            U -> 2
            L -> 3
