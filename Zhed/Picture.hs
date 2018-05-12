module Zhed.Picture where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Zhed.Puzzle

cellSize = 50 :: Double

renderSolutionSVG :: Puzzle -> [(Coord, Int, Dir)] -> FilePath -> IO ()
renderSolutionSVG puzzle solution path = renderSVG path sz diagram

  where
  Coord xmax ymax = puzzleBounds puzzle
  sz = mkSizeSpec (V2 (Just ( (fromIntegral xmax+1) * cellSize::Double))
                      (Just ( (fromIntegral ymax+1) * cellSize)))

  used = [ c | (c,_,_) <- solution ]

  subdiagrams
    = [ (c, targetSquare) | c <- puzzleTarget puzzle ]
    ++ [ (c, unusedSquare) | (c,_) <- puzzleSquares puzzle, c `notElem` used]
    ++ [ (c,  numLabel i <> pointedSquare dir) | (i,(c,_n,dir)) <- zip [1::Int ..] solution ]

  numLabel :: Int -> Diagram SVG
  numLabel i = fontSize (normalized 0.04) $ fc black $ text $ show i

  diagram :: Diagram SVG
  diagram =
    mconcat [ translate (V2 ( fromIntegral x * cellSize)
                            (-fromIntegral y * cellSize))
                        d
            | (Coord x y, d) <- subdiagrams ]

targetSquare :: Diagram SVG
targetSquare
  = pad 5
  $ fc blue
  $ square (40 :: Double)

unusedSquare :: Diagram SVG
unusedSquare
  = pad 5
  $ fc green
  $ square (40 :: Double)

pointedSquare :: Dir -> Diagram SVG
pointedSquare dir
  = rotateBy rot
  $ fc yellow
  $ strokeLocLoop
  $ fromVertices
  $ map (P . r2) [ (-20,-20), (0,-25), (20,-20), (20,20), (-20,20), (-20,-20) ]
  where
    rot = case dir of
            U -> (2/4)
            R -> (1/4)
            D -> (0/4)
            L -> (3/4)
