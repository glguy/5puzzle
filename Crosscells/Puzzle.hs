module Crosscells.Puzzle
  ( Op(..)
  , Constraint(..)
  , Puzzle
  , compile
  ) where

import Data.List
import Data.Maybe
import Data.Ord

import Crosscells.Region
import Crosscells.Tokens


type Puzzle = [(Constraint, [(Coord, Op)])]

data Op = Add Int | Mul Int
  deriving (Read, Show, Ord, Eq)

data Constraint = Count Int | Arith Int
  deriving (Read, Show, Ord, Eq)


compile :: [(Region, Token)] -> Puzzle
compile elts = mapMaybe (\(x,y) -> compile1 x y elts) elts

compileBox :: Token -> [Op]
compileBox (Plus  n) = [Add n]
compileBox (Times n) = [Mul n]
compileBox _ = []

compile1 :: Region -> Token -> [(Region, Token)] -> Maybe (Constraint, [(Coord, Op)])
compile1 region token =
  case token of
    Arrow dir -> compile1' cnstPredicate boxPredicate boxOrder
      where
        cnstPredicate = pointsTo (flipDirection dir) (topLeft region)
        boxPredicate = pointsTo dir (topLeft region)
        boxOrder = sortBy $
          case dir of
            U -> flip (comparing (coordRow . fst))
            D ->       comparing (coordRow . fst)
            L -> flip (comparing (coordCol . fst))
            R ->       comparing (coordCol . fst)

    Box -> compile1' predicate predicate id
      where
        predicate = contained region . topLeft

    _ -> const Nothing



compile1' cnstPredicate boxPredicate boxOrder elts =
  case rawConstraints of
    [c] -> Just c
    _   -> Nothing

  where
    boxes = boxOrder
      [ (topLeft reg, box) | (reg, elt) <- elts, boxPredicate reg, box <- compileBox elt]

    rawConstraints =
      [ c | (reg, elt) <- elts
          , cnstPredicate reg
          , c <- case elt of
                   Bracketed n -> [(Count n, boxes)]
                   Number    n -> [(Arith n, boxes)]
                   _           -> []
          ]
