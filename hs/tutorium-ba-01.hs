module T1 where

-- ghci
-- :l tutorium-ba.hs
-- :r

factors :: Integral a => a -> [a]
factors n = [a | a <- [1..n],n `mod` a == 0]

-- call with T1.min (Prelude.min conflict)
min :: Ord a => a -> a -> a
min a b = if a < b then a else b 

-- check_value -2 (parser conflict)
-- check_value (-2) -- use brackets for negative Integers 
check_value :: Int -> String
-- check_value :: (Num a, Ord a) => a -> [Char]
check_value a = if a == 0 then "null"
                else if a > 0 then "positive"
                     else "negative"

-- pattern matching (order!)
divide :: (Eq a, Fractional a) => a -> a -> a
divide _ 0 = error "Can't divide by 0" -- _ :: palceholder
divide a b = a/b

-- guards
check_value_2 :: Int -> String
check_value_2 0 = "null"
check_value_2 a
    | a > 0 = "positive"
    | a < 0 = "negative"

swim :: Double -> Bool -> String
swim temp sun
    | temp > 24 && sun = "go swimming"
    | temp > 20 && not sun = "wait for sun to show up"
    | temp > 15 && sun = "wait for temp to increase"
swim _ _ = "skip swimming for today"

my_abs :: (Num a, Ord a) => a -> a
my_abs a
    | a < 0 = -a
    | otherwise = a

-- recursion
fac_direct :: Int -> Int
fac_direct n = if n == 0 then 1 else n * fac_direct (n-1)

times_two :: [Int] -> [Int]
times_two a = map (*2) a

times_two_2 :: [Int] -> [Int]
times_two_2 a = [2*x | x <- a] -- list comprehension

my_sum :: [Int] -> Int
my_sum [] = 0
my_sum (x:xs) = x + my_sum(xs) 

my_len :: [Int] -> Int
my_len [] = 0
my_len (x:xs) = 1 + my_len(xs)

-- tuples
get_first :: (Int, Int) -> Int
get_first a = fst a

get_second :: (Int, Int) -> Int
get_second a = snd a

ggt :: Int -> Int -> Int
ggt a 0 = a
ggt a b = ggt b (a `mod` b)

-- extr a b c = a * x0^2 + b*x0 + c
--      | where x0 = -b/(2*a)
--
-- let {r=3; pi=3.14159} in r^2 * pi

-- True, False, ==, /=, not, &&, ||
