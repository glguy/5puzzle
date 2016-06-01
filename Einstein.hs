{-
FACTS:
1. There are 5 houses in 5 different colours.
2. In each house lives a person with a different nationality.
3. These 5 owners drink a certain beverage, smoke a certain brand of cigarette and keep a certain pet.
4. No owners have the same pet, brand of cigaratte, or drink.

CLUES:
1. The Brit lives in a red house
2. The Swede keeps a dog
3. The Dane drinks tea
4. The green house is on the left of the white house.
5. The green house owner drinks coffee.
6. The person who smokes Pall Mall keeps birds.
7. The owner of the yellow house smokes Dunhill.
8. The man living in the house right in the center drinks milk
9. The Norwegian lives in the first house.
10. The man who smokes Blend lives next to the one who keeps cats
11. The man who keeps horses lives next to the man who smokes Dunhill
12. The owner who smokes Camel drinks beer
13. The German smokes Marlborough.
14. The Norwegian lives next to the blue house
15. The man who smokes Blend has a neighbour who drinks water.

The question is, who keeps the fish?
-}
{-# Language DeriveGeneric #-}
module Main where

import Ersatz
import Select
import Booleans

import Data.List (intercalate)
import GHC.Generics (Generic)
import Prelude hiding (and, or, (&&), (||), not)

type SAssignment = ([Select Natl], [Select Drink], [Select Smoke], [Select Pet], [Select Color])
type Assignment = ([Natl], [Drink], [Smoke], [Pet], [Color])

data Natl  = Brit  | Swede   | Dane   | Norwgn | German deriving (Bounded, Enum, Show, Generic)
data Drink = Tea   | Beer    | Coffee | Water  | Milk   deriving (Bounded, Enum, Show, Generic)
data Smoke = Pall  | Dunhill | Camel  | Marl   | Blend  deriving (Bounded, Enum, Show, Generic)
data Pet   = Bird  | Dog     | Cat    | Horse  | Fish   deriving (Bounded, Enum, Show, Generic)
data Color = Green | Red     | Blue   | Yellow | White  deriving (Bounded, Enum, Show, Generic)

instance Equatable Natl
instance Equatable Drink
instance Equatable Smoke
instance Equatable Pet
instance Equatable Color

main :: IO ()
main =
  do (Satisfied, Just solution) <- solveWith minisat problem

     -- Verify that this solution is unique
     (Unsatisfied, solution') <- solveWith minisat $
       problem `checking` (/== encode solution)

     let _ = solution' `asTypeOf` Nothing -- type disambiguation

     printTable solution

printTable :: Assignment -> IO ()
printTable (natl,drink,smoke,pet,color) =
  do let write :: Show a => [a] -> IO ()
         write = putStrLn . intercalate "\t" . map show
     write natl
     write drink
     write smoke
     write pet
     write color

assignment :: MonadSAT s m => m SAssignment
assignment = (,,,,) <$> sel <*> sel <*> sel <*> sel <*> sel
  where
  sel :: (Bounded a, Enum a, MonadSAT s m) => m [Select a]
  sel = selectPermutation [minBound ..]

problem :: MonadSAT s m => m SAssignment
problem = assignment `checking` isValid

isValid :: SAssignment -> Bit
isValid (natl,drink,smoke,pet,color) = and
  [ match Brit   natl  Red     color
  , match Swede  natl  Dog     pet
  , match Dane   natl  Tea     drink
  , match German natl  Marl    smoke
  , match Green  color Coffee  drink
  , match Yellow color Dunhill smoke
  , match Bird   pet   Pall    smoke
  , match Beer   drink Camel   smoke
  , is Norwgn natl  !! 0
  , is Milk   drink !! 2

  , leftOf Green  color White   color
  , nextTo Blend  smoke Cat     pet
  , nextTo Horse  pet   Dunhill smoke
  , nextTo Norwgn natl  Blue    color
  , nextTo Blend  smoke Water   drink
  ]

match, leftOf, nextTo :: (Equatable a, Equatable b) => a -> [Select a] -> b -> [Select b] -> Bit
match  x xs y ys = is x xs === is y ys
leftOf x xs y ys = false : is x xs === is y ys ++ [false]
nextTo x xs y ys = leftOf x xs y ys || leftOf y ys x xs

is :: Equatable a => a -> [Select a] -> [Bit]
is x ys = map (encode x ===) ys
