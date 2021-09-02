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

{-
#####   Overloading #####

t -> t -> Bool  --  if the type t carries an equality   --  t -> t meaning both have the same type (t)
(==) :: Eq a => a -> a -> Bool)
2 == True = error   --  no point in comparing values of different types (will never be equal)

#####   Guards  #####

give alternatives in the definition of functions
Boolean expressions  -   express various cases

conditional expression
if <condition> then <m> else <m>

-}

myMax :: Integer -> Integer -> Integer
myMax a b 
  | a >= b    = a   --  | guard1
  | otherwise = b   --  | guard2  --  <otherwise> is not compulsory (required)

myMax2 :: Integer -> Integer -> Integer
myMax2 a b 
    = if a >= b then a else b

--  quickCheck prop_compareMax
prop_compareMax :: Integer -> Integer -> Bool
prop_compareMax a b =
    myMax a b == myMax2 a b

prop_max1, prop_max2 :: Integer -> Integer -> Bool

prop_max1 a b =
    a <= max a b && b <= max a b

prop_max2 a b =
    a == max a b || b == max a b

myMaxThree :: Integer -> Integer -> Integer -> Integer
myMaxThree a b c 
    | a >= b && a >= c = a
    | b >= c          = b
    | otherwise       = c

--  myMaxThree 6 (3+7) 2    --  (3+7) will only be calculated once (lazy evaluation - haskell is lazy!)

--  redefine prelude functions
--  import Prelude hiding (max,min)

{-
#####   Characters: Char    #####

literal characters are written in single quotes: 't'
'\t'    tab
'\n'    newline
'\\'    backslash (\)
'\''    single quote (')
'\"'    double quote (")

ASCII = characters as integers
'A' to 'Z'  --  65 to 90
'a' to 'z'  --  97 to 122

e.g. code 9: '\9' = '\t

fromEnum :: Char -> Int     --  fromEnum 'a' = 97
toEnum   :: Int  -> Char

-}

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

--  toUpper 'c' = 'C'
toUpper :: Char -> Char
toUpper c = toEnum (fromEnum c + offset)

--  digits 0-9  --  codes 48-57
isDigit :: Char -> Bool
isDigit c = ('0' <= c) && (c <= '9')

charToNum :: Char -> Int
charToNum c
    | isDigit c = fromEnum c - (fromEnum '\48')   --  offset
    | otherwise = 0

{-
#####   Strings: String    #####

String :: [Char]    --  sequence of strings of characters, between double quotes ("test")

--  resolves escape characters and loses double quotes
putStr :: String -> IO ()   --  performs output operation

++  join operator   --  (++) :: [a] -> [a] -> [a]   --  "lo"++"l" = "lol"

show        value to String
read        String to value     --  add type for output

--  (read "0.1") :: Float
-}

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++ b ++ c 

--  quickCheck prop_concatThreeStrings = +++ OK, passed 100 tests.
prop_concatThreeStrings :: String -> String -> String -> Bool
prop_concatThreeStrings a b c =
    length (onThreeLines a b c) == (length a) + (length b) + (length c)

{-
#####   Floating-point numbers: Float   #####

-}



{-
#####   Syntax  #####


#####   Names   #####


#####   Operators   #####


-}
