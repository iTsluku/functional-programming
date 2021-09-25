module Testing where

import Test.QuickCheck
import Test.HUnit

{-
QuickCheck  --  test properties of functions using randomly generated data

black box testing   --  use the specification of a function and devise test data according to that
white box testing   --  use form of the function definition itself to guide our choice of test data

testing alone can not assure us that a function is correct!
-}

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c 

--  black box testing
--
--  partion the inputs into testing groups
--  pay particular attention to special cases

--  msg if test fails, expected result, evaluated expression 
testMax1 = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))

--  runTestTT testsMax = 
--
--Cases: 4  Tried: 4  Errors: 0  Failures: 0
--Counts {cases = 4, tried = 4, errors = 0, failures = 0}
testsMax = TestList [testMax1,testMax2,testMax3,testMax4]

--  white box testing
--
--  if a function uses recursion we should test the zero case, the one case
--  and the general case

--  Int is a fixed-size representation of integers, if the number becomes
--  big enough, it wraps around "the negative" -> use Integer for "real integers".

allEqual :: Integer -> Integer -> Integer -> Bool
allEqual a b c = a == b && b == c

testEqual1 = TestCase (assertEqual "for: allEqual 2 2 2" True (allEqual 2 2 2))
testEqual2 = TestCase (assertEqual "for: allEqual 2 3 2" False (allEqual 2 3 2))
testEqual3 = TestCase (assertEqual "for: allEqual (-2) 3 3" False (allEqual (-2) 3 3))

--  runTestTT testsEqual
testsEqual = TestList [testEqual1,testEqual2,testEqual3]

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = (getAbove a (getAvg a b c)) + (getAbove b (getAvg a b c)) + (getAbove c (getAvg a b c))
        where
            getAvg :: Integer -> Integer -> Integer -> Integer
            getAvg a b c = div (a + b + c) 3
            getAbove :: Integer -> Integer -> Integer
            getAbove x avg
                | x > avg   = 1
                | otherwise = 0

testNrAboveAvg1 = TestCase (assertEqual "for: howManyAboveAverage 4 4 1" 2 (howManyAboveAverage 4 4 1))
testNrAboveAvg2 = TestCase (assertEqual "for: howManyAboveAverage 4 6 8" 1 (howManyAboveAverage 4 6 8))
testNrAboveAvg3 = TestCase (assertEqual "for: howManyAboveAverage 4 4 4" 0 (howManyAboveAverage 4 4 4))

--  runTestTT testsNrAboveAvg =
--  Cases: 3  Tried: 3  Errors: 0  Failures: 0
--  Counts {cases = 3, tried = 3, errors = 0, failures = 0}
testsNrAboveAvg = TestList [testNrAboveAvg1,testNrAboveAvg2,testNrAboveAvg3]

