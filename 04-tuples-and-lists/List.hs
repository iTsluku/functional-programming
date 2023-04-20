module List where

import Data.Char
import Test.QuickCheck

{-
combine an arbitrary number of values (same type) into a single object.
same type -> allows type check prior to execution -> type errors will be found before a program is actually executed.
haskell contains many built-in list functions.
list comprehension: flexible notation for transforming and selecting elements of lists.
-}
type ShopItem = (String, Int)

type Basket = [ShopItem]

--  [("Salt: 1kg",69),("plain crisps",25)] :: Basket
--  type String = [Char]    --  special case of list type (synonyms fot the type which it names -- like ShopItem)
--  type introduces a definition of a type, not a value
--  type names begin with capital letters
l1 = [1, 2, 3, 4, 5] :: [Integer]

l2 = [True] :: [Bool]

l3 = ['a', 'b', 'c'] :: String

l4 = "abc" :: String

l5 = [[1, 2], [0], [13, 2, 5]] :: [[Integer]]

--  [] :: empty list
--  String type
--  type String = [Char]
{-
'\n'    newline
'\t'    tab
'++'    concat (join Strings)
-}
--  show (2+3)  = "5"   --  from value to String
--  read "True" = True  --  from String to value
--  read can be combined with giving a type 
--  (read "3") :: Integer
--  length [1..10] = 10
--  list comprehension
l6 = [2 * n | n <- [1 .. 10]] --  '<-' meaning the mathematical 'element of'

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

--  l7 = [False,True,False,True,False,True,False,True,False,True] 
l7 = [isEven x | x <- [1 .. 10]]

l8 = [even x | x <- [1 .. 10]]

--  x <- xs is called the generator, because it generates the data from
--  wich the results are built on.
--  l9 = [8,12,16,20]
l9 = [2 * x | x <- [1 .. 10], even x, x > 3]

-- even x and x>3 fucntion as 'test'
addPairs :: [(Integer, Integer)] -> [Integer]
addPairs l = [m + n | (m, n) <- l]

--  l10 = [3,7,12]
l10 = addPairs [(1, 2), (5, 2), (9, 3)]

addOrdPairs :: [(Integer, Integer)] -> [Integer]
addOrdPairs l = [m + n | (m, n) <- l, m < n]

--  req import Data.Char
--  find digits in String
digits :: String -> String
digits s = [c | c <- s, isDigit c]

allEven :: [Integer] -> Bool
allEven xs = xs == [x | x <- xs, even x]

allOdd :: [Integer] -> Bool
allOdd xs = xs == [x | x <- xs, odd x]

allOdd2 :: [Integer] -> Bool
allOdd2 [] = True
allOdd2 (x:xs)
  | even x = False
  | otherwise = allOdd2 xs

--  extract all singelton elements from a list of lists
--  sings [[2],[3]] = [2,3]
--  sings [[2,4],[3]] = [3]
sings :: [[Integer]] -> [Integer]
sings xss = [x | [x] <- xss]

capitalize :: String -> String
capitalize s = [toUpper x | x <- s]

--  capitalize chars and remove non-letters
capitalizeLetters :: String -> String
capitalizeLetters s = capitalize [x | x <- s, (not . isDigit) x]

--  defined for n>=0
divisors :: Integer -> [Integer]
divisors 0 = []
divisors n = [x | x <- [1 .. n], rem n x == 0] --  rem returns reminder

isPrime :: Integer -> Bool
isPrime n
  | n == 0 = False
  | n == 1 = True
  | otherwise = checkRem n (n - 1)
  where
    checkRem :: Integer -> Integer -> Bool
    checkRem n x
      | x == 1 = True
      | rem n x == 0 = False
      | otherwise = checkRem n (x - 1)

createPrimeTuple :: Integer -> (Integer, Bool)
createPrimeTuple n
  | isPrime n = (n, True)
  | otherwise = (n, False)

--  [x | x <- getPrimeTuples 10 , snd x] = [(1,True),(2,True),(3,True),(5,True),(7,True)]
getPrimeTuples :: Integer -> [(Integer, Bool)]
getPrimeTuples 0 = []
getPrimeTuples n = [createPrimeTuple x | x <- [1 .. n]]

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x <- xs, x == n]

--  or import Prelude hiding (elem)
myElem :: Integer -> [Integer] -> Bool
myElem n xs = (not . null) (matches n xs)

--  putStrLn (onSeperateLines ["adSA","abc"])
--  adSA
--  abc
onSeperateLines :: [String] -> String
onSeperateLines [] = ""
onSeperateLines (x:xs) =
  if null xs
    then x
    else x ++ ['\n'] ++ onSeperateLines xs

duplicate :: String -> Integer -> String
duplicate _ 0 = ""
duplicate s 1 = s
duplicate s n = s ++ duplicate s (n - 1)

--  pushRight "abc" = "         abc"
linelength = 12 :: Int

pushRight :: String -> String
pushRight s
  | linelength <= 0 || linelength > 150 = error "linelength not in range."
  | length s < linelength =
    duplicate " " (toInteger (linelength - length s)) ++ s
  | otherwise = s

--  lib database example
type Person = String

type Book = String

--  list of perso(Person,Book) pairs    --  ("Alice","B") :: Alice has
--  borrowed the book called B.
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase =
  [ ("Alice", "Tintin")
  , ("Anna", "Little Women")
  , ("Alice", "Asterix")
  , ("Rory", "Tintin")
  ]

--  given a person, check what books got borrowed
books :: Database -> Person -> [Book]
books db pName = [book | (name, book) <- db, name == pName]

--  given a book, find the borrowers - assumed there may be more than one
--  copy of any book
borrowers :: Database -> Book -> [Person]
borrowers db bName = [name | (name, book) <- db, book == bName]

--  given a book, find out wether it is borrowed
borrowed :: Database -> Book -> Bool
borrowed db bName = (not . null) [book | (name, book) <- db, book == bName]

--  given a person, find out how many books that person borrowed
numBorrowed :: Database -> Person -> Int
numBorrowed db pName = length [book | (name, book) <- db, name == pName]

--  loan a book to a person
makeLoan :: Database -> Person -> Book -> Database
makeLoan db pName bName = (pName, bName) : db

--  return loaned book
returnLoan :: Database -> Person -> Book -> Database
returnLoan db pName bName = rmFstOcc db (pName, bName)
  where
    rmFstOcc :: Database -> (String, String) -> Database
    rmFstOcc [] _ = []
    rmFstOcc (x:xs) (pName, bName)
      | fst x == pName && snd x == bName = xs
      | otherwise = x : rmFstOcc xs (pName, bName)

returnLoan2 :: Database -> Person -> Book -> Database
returnLoan2 db pName bName = [pair | pair <- db, pair /= (pName, bName)]

-- makeLoan [] "ya" "yeet" = [("ya","yeet")] 
test1 :: Bool
test1 = borrowed exampleBase "Asterix"

test2 :: Database
test2 = makeLoan exampleBase "Alice" "Rotten Romans"

--  In ghci, 'it' is a name automatically bound to the result of the last
--  expression you evaluated.
--   test2
--   makeLoan it "Rory" "Godzilla"
--   returnLoan it "Alice" "Rotten Romans"
--  loan bName to pName, then lookup the books loaned by pName and check
prop_db1 :: Database -> Person -> Book -> Bool
prop_db1 db pName bName = elem bName (books (makeLoan db pName bName) pName)

--  if we return the loan of bName to pName and then check book loans for
--  pName -> bName should not be in that list
prop_db2 :: Database -> Person -> Book -> Bool
prop_db2 db pName bName =
  elem bName (books (returnLoan db pName bName) pName) == False
-- isAlpha ch
-- toUpper ch
