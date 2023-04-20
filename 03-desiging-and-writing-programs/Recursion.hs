module Recursion where

import Test.QuickCheck

{-
definition of a function refering itself

primitive recursion:    given the value at 0 and explaining how to get from n-1 to n
-}

fac :: Integer -> Integer
fac n
  | n == 0    = 1                --  base case
  | n > 0     = n * fac (n-1)    --  recursion step  
  | otherwise = error "fac only defined on natural numbers."

--  product of all natural numbers in [n,m], given n and m are natural numbers
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n  
  | m < 0 || n < 0   = error "rangeProduct only defined on natural numbers."
  | n < m || m == 0  = 0
  | otherwise       = rProd m n
    where
        rProd :: Integer -> Integer -> Integer
        rProd m n 
            | n < m     = 1
            | n == m    = n
            | otherwise = n * m * rProd (m+1) (n-1)

fac2 :: Integer -> Integer
fac2 0 = 1
fac2 n = rangeProduct 1 n

--  quickCheck prop_fac = +++ OK, passed 100 tests.
prop_fac :: Integer -> Bool
prop_fac n
  | n < 0     = True
  | otherwise = fac n == fac2 n 

sumFac :: Integer -> Integer
sumFac n
  | n == 0    = 1
  | n > 0     = fac n + sumFac (n-1)
  | otherwise = error "sumFac only defined on natural numbers."

--  quickCheck prop_sumFac = +++ OK, passed 100 tests.
prop_sumFac :: Integer -> Bool
prop_sumFac n 
  | n < 1     = True  --  <1 because of (n-1) in guard
  | otherwise = sumFac n == fac n + sumFac (n-1)

--  sumFacFun fac 3 = 10
--  f :: function as argument
sumFacFun :: (Integer -> Integer) -> Integer -> Integer
sumFacFun f n 
    | n == 0    = f 0
    | n > 0     = sumFacFun f (n-1) + f n
    | otherwise = error "sumFacFun only defined on natural numbers."

prop_sumFac2 :: Integer -> Bool
prop_sumFac2 n
    | n < 1     = True
    | otherwise = sumFacFun fac n == sumFac n 

intSqrt :: Integer -> Integer
intSqrt n 
    | n == 0    = 0
    | n == 1    = 1
    | n > 1     = getMax n n
    | otherwise = error "intSqrt only defined on natural numbers."
        where
            getMax :: Integer -> Integer -> Integer
            getMax n m
              | n * n <= m = n
              | otherwise  = getMax (n-1) m

prop_intSqrt :: Integer -> Bool
prop_intSqrt n
    | n < 0          = True
    | intSqrt n <= n = True
    | otherwise      = False

testFun1 :: Integer -> Integer
testFun1 0 = 0
testFun1 1 = 44
testFun1 2 = 17
testFun1 3 = 72
testFun1 4 = 32
testFun1 5 = 92
testFun1 _ = 0

--  getMaxOfRange testFun1 4 = 72
getMaxOfRange :: (Integer -> Integer) -> Integer -> Integer
getMaxOfRange f n 
    | n < 0  = error "getMaxOfRange n only defined on natural numbers."
    | n == 0 = f 0
    | otherwise = getMax f n 1 (f 0)
        where
            getMax :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
            getMax f n i m    --  m :: temp max
              | i == n = if f i > m then f i else m
              | otherwise = if f i > m then getMax f n (i+1) (f i) else getMax f n (i+1) m

testFun2 :: Integer -> Integer
testFun2 0 = 10
testFun2 1 = 44
testFun2 2 = 17
testFun2 3 = 72
testFun2 4 = 0
testFun2 5 = 92
testFun2 _ = 0

holdsZeroInRange :: (Integer -> Integer) -> Integer -> Bool
holdsZeroInRange f n
    | n < 0     = error "holdsZeroInRange n only defined on natural numbers."
    | n == 0    = f n == 0
    | otherwise = if f n == 0 then True else holdsZeroInRange f (n-1)

--  general forms of recursion

fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | n > 1  = fib (n-2) + fib (n-1)

--  integer division

remainder :: Integer -> Integer -> Integer
remainder a b
    | b == 0        = error "denominator can not be zero."
    | a < 0 || b < 0 = error "remainder only defined on natural numbers."
    | a < b         = a
    | otherwise     = remainder (a-b) b

divide :: Integer -> Integer -> Integer
divide a b
    | b == 0        = error "denominator can not be zero."
    | a < 0 || b < 0 = error "remainder only defined on natural numbers."
    | a < b         = 0
    | otherwise     = 1 + divide (a-b) b

highestCommonFactor :: Integer -> Integer -> Integer
highestCommonFactor a b 
    | a >= b    = getFactor a b
    | otherwise = getFactor b a
        where
            getFactor :: Integer -> Integer -> Integer
            getFactor a b 
                | b == 0             = 0
                | remainder a b == 0 = b
                | otherwise          = getFactor a (b-1)


