module ProgramDesign where

import Test.QuickCheck hiding (Result)

{-
#####   local definitions   #####
solving problems using functions (step by step)
(design is what we do before we start writing detailed haskell code)

--  Do i understand what i need to do?
--  What do i already know? How can i use this information?
--  Can i break down the problem down into smaller parts? (divide and conquer)
--  What if i had any functions i wanted? Which could i use in writing the
--  solution? (Start -> what if functions -> Goal)  
--  top-down, breaking it into smaller problems.. making the overall
--  problem solvable in a series of smaller "jumps"
--
-}

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

--  infix
maxThree2 :: Integer -> Integer -> Integer -> Integer
maxThree2 x y z = (x `max` y) `max` z

--  quickCheck prop_compareMaxThree
prop_compareMaxThree :: Integer -> Integer -> Integer ->  Bool
prop_compareMaxThree x y z =
    maxThree x y z == maxThree2 x y z

between :: Integer -> Integer -> Integer -> Bool
between x y z = (y >= x) && (y <= z)

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z 
    | between x y z = y
    | between z y x = y
    | between y x z = x
    | between z x y = x
    | otherwise     = z

prop_middleNumber1 :: Integer -> Integer -> Integer -> Bool
prop_middleNumber1 x y z =
    ((middleNumber x z z <= maxThree x y z) && (middleNumber x z z >= (min (min x y) z )))
    
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d = max (maxThree a b c) d

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder x y z 
    | x <= y && y <= z = True
    | otherwise       = False

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z 
  | x == y && y == z = 3
  | x == y || y == z = 2
  | otherwise       = 0

--sq :: Integer -> Integer
--sq n = n*n

--  local definitions
--  where  --  layout is significant
sumSquares :: Integer -> Integer -> Integer
sumSquares n m = sqN + sqM
    where  
        sqN = sq n
        sqM = sq m
        sq :: Integer -> Integer    --  local
        sq n = n*n


isEven, isOdd :: Int -> Bool

isOdd n
    | n <= 0    = False
    | otherwise = isEven (n-1)
isEven n
    | n < 0     = False
    | n == 0    = True
    | otherwise = isOdd (n-1)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z = (maxV,occ)
    where
        maxV = fromIntegral (maxThree (toInteger x) (toInteger y) (toInteger z))
        checkOcc :: Int -> Int -> Int -> Int -> Int
        checkOcc x y z c 
          | fromIntegral ((howManyEqual (toInteger x) (toInteger y) (toInteger z))) == 3  = 3
          | (x == y && x == maxV) || (y == z && y == maxV) || (x == z && x == maxV)       = 2
          | otherwise                                                                     = 1
        occ  = checkOcc x y z maxV

{-
#####   data types (enumerated types)  #####

--  rock, paper, scissors
same -> neither wins
rock defeats scissors
paper defeats rock
scissors defeut paper

-}

data Move = Rock | Paper | Scissors deriving (Eq)   --  deriving (Show,Eq)

-- Showing Moves in an abbreviated form.
instance Show Move where
    show Rock     = "r"
    show Paper    = "p"
    show Scissors = "s"

--  tells us the move to beat a particular move
beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

--  tells us the move that will lose against a particular move
lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock
lose _     = Paper
--  wildcard '_' instead of Scissors, because this case will only be matched, when all the others do not

data Result = Win | Lose | Draw deriving (Show,Eq)

-- POV Player1 (x: Player1 Move, y: Player2 Move)
outcome :: Move -> Move -> Result
outcome x y
    | x == y        = Draw
    | y == (lose x) = Win
    | otherwise     = Lose


-- For QuickCheck to work over the Move type.
instance Arbitrary Move where
    arbitrary = elements [Rock, Paper, Scissors]

prop_gameLogic :: Move -> Bool
prop_gameLogic m =
    beat m /= lose m


