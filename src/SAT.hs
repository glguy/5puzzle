module SAT where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Foldable(toList)
import Control.Monad(foldM)

import qualified Ersatz.Solution as Ersatz
import qualified Ersatz.Problem as Ersatz
import qualified Ersatz.Internal.Formula as Ersatz


solver :: Applicative m => Ersatz.Solver Ersatz.SAT m
solver (Ersatz.SAT _ f _) =
  pure $
  case check (map toCl (toList (Ersatz.formulaSet f))) of
    Nothing -> (Ersatz.Unsatisfied, IntMap.empty)
    Just xs -> ( Ersatz.Satisfied, IntMap.fromList (Map.toList xs))
  where
  toCl :: Ersatz.Clause -> Clause
  toCl cl = Map.fromList
              [ (abs x, if x > 0 then Pos else Neg)
                      | x <- IntSet.toList (Ersatz.clauseSet cl) ]

type Var      = Int
data Polarity = Neg | Pos deriving (Eq,Ord,Show)
data Lit      = Lit { litPolarity :: Polarity, litVar :: Var }
                  deriving Show

type Assignment = Map Var Bool

polarity :: Polarity -> Bool -> Bool
polarity p = case p of
               Pos -> id
               Neg -> not

-- | The value of a varible in the given assignment, if any.
lookupVar :: Var -> Assignment -> Maybe Bool
lookupVar = Map.lookup

type Clause = Map Var Polarity

polarityOf :: Var -> Clause -> Polarity
polarityOf x c = c Map.! x


data Clause1  = Clause1 Var     Clause deriving Show
data Clause2  = Clause2 Var Var Clause deriving Show
type ClauseId = Int

data State = State
  { assigned    :: Assignment
  , unassigned  :: [Var]
  , monitored   :: Map Var [ClauseId]
  , incomplete  :: Map ClauseId Clause2
  , guesses     :: [(Var,State)]
  } deriving Show



check :: [Clause] -> Maybe Assignment
check cs =
  do (vs, c1,c2) <- foldM preProc (Set.empty, [],[]) cs
     let inc = zip [ 0 .. ] c2 :: [(Int,Clause2)]
         s   = State { assigned   = Map.empty
                     , unassigned = Set.toList vs
                     , monitored  = Map.fromListWith (++)
                                      [ v | (i, Clause2 x y _) <- inc
                                          , v <- [ (x,[i]), (y,[i]) ]
                                      ]
                     , incomplete = Map.fromList inc
                     , guesses    = []
                     }
     checkSat c1 s


  where
  preProc (vs,c1,c2) c =
    do let vs1 = Set.union (Map.keysSet c) vs
       ((x,_),rest) <- Map.minViewWithKey c
       case Map.minViewWithKey rest of
         Nothing    -> return (vs1, Clause1 x c : c1, c2)
         Just ((y,_),_) -> return (vs1, c1, Clause2 x y c : c2)


checkSat :: [Clause1] -> State -> Maybe Assignment
checkSat sing s =
  case sing of
    []  -> guess s
    Clause1 x c : more ->
      case lookupVar x (assigned s) of
        Just b
          | polarity p b -> checkSat more s
          | otherwise    -> backtrack s
        Nothing -> setVar more x (polarity p True) s
      where
      p = polarityOf x c


backtrack :: State -> Maybe Assignment
backtrack s =
  case guesses s of
    [] -> Nothing
    (x,s1) : _ -> checkSat [Clause1 x learn] s1
  where
  learn = Map.fromList [ (x,Neg) | (x,_) <- guesses s ]



guess :: State -> Maybe Assignment
guess s =
  case unassigned s of
    x : _ -> setVar [] x True s { guesses = (x,s) : guesses s }
    []    -> return (assigned s)


setVar :: [Clause1] -> Var -> Bool -> State -> Maybe Assignment
setVar sing v b s0 =
  let s = s0 { assigned = Map.insert v b (assigned s0)
             , unassigned = List.delete v (unassigned s0)
             }
  in
  case Map.lookup v (monitored s) of
    Nothing -> checkSat sing s
    Just xs ->
      let (sings,s1) = updClauses v b s xs
      in checkSat (sing ++ sings) s1   -- does order matter?

data SetVar2Result = Complete | Watched Clause2 | Singleton Clause1

updClauses :: Var -> Bool -> State -> [ClauseId] -> ([Clause1],State)
updClauses x v s0 = foldr upd ([],s0)
  where
  agn = assigned s0

  upd cid done@(sing,s) =
    case setVarInClause2 agn x v (incomplete s Map.! cid) of
      Complete    -> done
      Watched w   -> (sing, s { incomplete = Map.insert cid w (incomplete s) })
      Singleton c -> (c : sing, s)


setVarInClause2 :: Assignment -> Var -> Bool -> Clause2 -> SetVar2Result
setVarInClause2 agn x v (Clause2 a b c)
  | polarity (polarityOf this c) v = Complete
  | otherwise = foldr pick (Singleton (Clause1 other c)) (Map.toList c)
  where
  (this,other) = if x == a then (a,b) else (b,a)

  pick (v1,p1) notMe
    | v1 == this  = notMe
    | v1 == other = notMe
    | Just y <- lookupVar v1 agn = if polarity p1 y then Complete else notMe
    | otherwise = Watched (Clause2 v1 other c)


{-
A clause with at least one literal set to true is "complete".


case A: All incomplete clauses have two unasigned literals:
  * Set one literal to true (reason: guess)

case B: There are incomplete singleton clauses.
      (one unasigned literal, all other literals are false)

  * Set the unasigned literal to true
      (reason: reason of last literal that become false)


case C: There are unsatisfiable clauses.
      (all literals are false).

  * We need to change one of our guesses: we've learnt that this
    guess is incompatible with the other gusses we made.

-}

{-

Implementing backtracking on contradition:

    We can implement the change of guess as follows:
      1. backtrack the state to just before making the guess
      2. add a new "learned" clause, to recored what we found
        (G1 /\ G2 /\ G3 => not G4)
        (not G1 \/ not G2 \/ not G3 \/ not G4)
        where G1..G3 are all the other guesses,
              and G4 is the guess being changed.

      3. By design, this new learned clause is a singleton clause,
         so if we add it to the set of clauses, we are turning case (A)
         into case (B), and so we can continue as neccessary.

Detecting singleton clauses:

  To avoid having to look trough all clauses to see if we are in case
  A, B, or C, we can use the following trick. Start by preprocessing the
  problem:

    0. If we have any empty clauses we fail.
    1. If we have any singleton clauses (before any guesses)
       we assign those literals to true, and then we can eliminate the clauses.

  As a result, all clauses will have at least two literals.
    1. For each clause, pick two literals to "monitor".
    2. When setting a literal, we only need to check clauses
       for which the literal's variable is being monitored:
        - If the monitored literal becomes true, we don't need to do anything,
          the clause became "complete"
        - If the monitored literal became false, we need to find antoher
          literal to monitor:
          - if we are successful, then we still have two unasigned clauses,
            so nothing else to do
          - if there is no such literal, then:
            - if we are in a singleton clause, then we found a contradiciont
            - otherwise, we found a new singleton clause
-}


