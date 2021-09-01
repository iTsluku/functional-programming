module BasicTypes where

import Test.QuickCheck

{-

Integer, Int    --  numerical types
Float           --  floating-point fractional numbers
Bool            --  Boolean values (also used to give choices in function definitions by means of guards)
Char            --  Characters
String          --  Strings of letters

-}

{- 
#####   The Booleans: Bool    ##### 

&&  and
||  or  (inclusive)
not not
==  equality
/=  inequality

-}

exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr x y = (x || y) && not (x && y)

--  exclusive Or (XOR) using pattern matching
exOr :: Bool -> Bool -> Bool
exOr True x  = not x
exOr False x = x

--  Testing using QuickCheck properties
--  quickCheck prop_exOrs = +++ OK, passed 100 tests.
prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y =
    exclusiveOr x y == exOr x y

--  quickCheck prop_exOrs2 = +++ OK, passed 100 tests.
prop_exOrs2 :: Bool -> Bool -> Bool
prop_exOrs2 x y = exOr x y == (x /= y)

prop_exOrs3 :: Bool -> Bool -> Bool
prop_exOrs3 x y = exOr x y == ((x && not y) /= (not x && y))

exOr2 :: Bool -> Bool -> Bool
exOr2 True True   = False 
exOr2 True False  = True
exOr2 False True  = True
exOr2 False False = False

prop_exOrs4 :: Bool -> Bool -> Bool
prop_exOrs4 x y =
    exOr2 x y == exOr x y

-- import Prelude hiding (&&)
myAnd :: Bool -> Bool -> Bool
myAnd True True   = True
myAnd _ _         = False

prop_myAnd :: Bool -> Bool -> Bool
prop_myAnd x y =
    myAnd x y == (x && y)   -- brackets!

{-
#####   The integers: Integer and Int   #####

whole numbers, positive, zero and negative (used for counting)

arithmetics using:
+       sum
*       product
^       power
-       difference (-a (präfix), a-b (infix))
div     whole number division (div 3 2 = 1, 3 `div` 2 = 1 (using ``-backquotes))
mod     remainder from whole number division (mod 3 2 = 1, 3 `mod` 2 = 1)
abs     absolute value of an integer
negate  function to change a sign of an integer

negative literals
negate (-34) = 34   --  enclose with paranthesis (else compiler error, because of infix operator confusion )
(negate - 34 = error)    

relational operators
>, >=, ==, /=, <=, <

Int: integers in fixed aount of space -> finite range of of integers
maxBound :: Int = 9223372036854775807

convert Int, Integer
fromInteger :: Integer -> Int
toInteger   :: Int -> Integer

-}

threeEquals :: Integer -> Integer -> Integer -> Bool
threeEquals a b c = (a == b) && (b == c)

notThreeEquals :: Integer -> Integer -> Integer -> Bool
notThreeEquals a b c = not ((a == b) && (b == c))

--  quickCheck prop_threeEquals = +++ OK, passed 100 tests.
prop_threeEquals :: Integer -> Integer -> Integer -> Bool
prop_threeEquals a b c =
    threeEquals a b c /= notThreeEquals a b c 

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = not ((a == b) || (b == c) || (a == c))

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = (threeEquals a b c) && (c == d) 

prop_fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
prop_fourEqual a b c d =
    fourEqual a b c d == ((a == b) && (b == c) && (c == d))
