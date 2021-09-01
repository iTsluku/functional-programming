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
#####   Bool    ##### 

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


