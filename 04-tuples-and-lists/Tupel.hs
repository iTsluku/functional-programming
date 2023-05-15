module Tupel where

import Test.QuickCheck

{-
combine a fixed number of fixed types (might be dfferent) into a sigle object.

pairs, triples, quadruples, quintuples, sextuples, ...
-}
type ShopItem = (String, Int)

--  ("Salt: 1kg",69) :: ShopItem
minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
  | x >= y = (y, x)
  | otherwise = (x, y)

addPair :: (Integer, Integer) -> Integer
addPair (x, y) = x + y

--  addPair (2,7) = 9
--  2 matches to x
--  7 matches to y
shift :: ((Integer, Integer), Integer) -> (Integer, (Integer, Integer))
shift ((x, y), z) = (x, (y, z))

--  name ("Salt: 1kg",69) = "Salt: 1kg"
name :: ShopItem -> String
name (n, p) = n

--  price ("Salt: 1kg",69) = 69 
price :: ShopItem -> Int
price (n, p) = p

--  fst (x,y) = x
--  snd (x,y) = y
addPair2 :: (Integer, Integer) -> Integer
addPair2 p = fst p + snd p

--   addPair3 (2,7) = 9
addPair3 :: (Integer, Integer) -> Integer
addPair3 = uncurry (+)

--   pairProduct (2,7) = 14
pairProduct :: (Integer, Integer) -> Integer
pairProduct = uncurry (*)

--  efficient fib
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fib (n - 1) + fib (n - 2)
  | otherwise = error "fib is only defined on natural numbers."

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
  | n == 0 = (0, 1)
  | otherwise = fibStep (fibPair (n - 1))

--  passing the output of fibPair to the input of fst, which picks the
--  first component of the tupel/pair.
fastFib :: Integer -> Integer
fastFib = fst . fibPair --  composing both functions

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
  | x == y = (x, 2)
  | x > y = (x, 1)
  | otherwise = (y, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs x y z = (getMax x y z, getOcc (getMax x y z) x y z)
  where
    getMax :: Integer -> Integer -> Integer -> Integer
    getMax x y z = fst (maxOccurs (fst (maxOccurs x y)) z)
    getOcc :: Integer -> Integer -> Integer -> Integer -> Integer
    getOcc m x y z =
      (snd (maxOccurs m x) - 1) + (snd (maxOccurs m y) - 1) +
      (snd (maxOccurs m z) - 1)

--  orderTriple (2,(-9),3) = (-9,2,3)
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z)
  | x == min (min x y) z =
    if y <= z
      then (x, y, z)
      else (x, z, y)
  | x == max (max x y) z =
    if y <= z
      then (y, z, x)
      else (z, y, x)
  | otherwise =
    if y <= z
      then (y, x, z)
      else (z, x, y)

prop_addPair :: (Integer, Integer) -> Bool
prop_addPair (x, y) =
  addPair (x, y) == addPair2 (x, y) && addPair2 (x, y) == addPair3 (x, y)

--  product types
data People =
  Person Name Age
  deriving (Eq, Show) --  derive definitions like equality using "deriving"

type Name = String

type Age = Int

--  showPerson (Person "Andreas" 24) = "Andreas -- 24"
--  binary constructor - takes 2 values to form a value of type Person
showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n --  show gives textual form of an Int

--  :t Person = Person :: Name -> Age -> People
data Shape
  = Circle Float
  | Rectangle Float Float
  deriving (Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r ** 2
area (Rectangle h w) = h * w
--  data definitions create a new type and are often used recursively
--  synonym definition cannot be recursive - they are simply shorthanded
--  and can always be expanded out (removed from the program)
