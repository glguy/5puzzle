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
module Main where

import Ersatz
import Select
import Booleans

import Data.List (intercalate)
import Prelude hiding (and, or, (&&), (||), not)

main =
  do (Satisfied, Just x) <- solveWith minisat problem
     mapM_ (putStrLn . intercalate "\t") x

problem :: MonadSAT s m => m [[Select String]]
problem = traverse selectPermutation
  [["Brit" ,"Swede"  ,"Dane"  ,"Norwgn","German"]
  ,["Tea"  ,"Beer"   ,"Coffee","Water" ,"Milk"  ]
  ,["Pall" ,"Dunhill","Camel" ,"Marl"  ,"Blend" ]
  ,["Bird" ,"Dog"    ,"Cat"   ,"Horse" ,"Fish"  ]
  ,["Green","Red"    ,"Blue"  ,"Yellow","White" ]
  ] `checking` isValid

isValid :: [[Select String]] -> Bit
isValid [natl,drink,smoke,pet,color] = and
  [ match "Brit"   natl  "Red"     color
  , match "Swede"  natl  "Dog"     pet
  , match "Dane"   natl  "Tea"     drink
  , match "German" natl  "Marl"    smoke
  , match "Green"  color "Coffee"  drink
  , match "Yellow" color "Dunhill" smoke
  , match "Bird"   pet   "Pall"    smoke
  , match "Beer"   drink "Camel"   smoke
  , is "Milk"   (drink !! 2)
  , is "Norwgn" (natl  !! 0)

  , leftOf "Green"  color "White"   color
  , nextTo "Blend"  smoke "Cat"     pet
  , nextTo "Horse"  pet   "Dunhill" smoke
  , nextTo "Norwgn" natl  "Blue"    color
  , nextTo "Blend"  smoke "Water"   drink
  ]

match, leftOf, nextTo :: Eq a => a -> [Select a] -> a -> [Select a] -> Bit
match  x xs y ys = map (is x) xs === map (is y) ys
leftOf x xs y ys = map (is x) xs === map (is y) (tail ys) ++ [false]
nextTo x xs y ys = leftOf x xs y ys || leftOf y ys x xs

is :: Eq a => a -> Select a -> Bit
is = selectEq . pure
