module T2 where

-- lists
-- empty list: []
-- add element (front):  1 : [1,2,3] = [1,1,2,3]
-- get n-th element: [0,1,2,3,4]!!2 = 2
-- [1..4] = [1,2,3,4]

reverse_list :: [a] -> [a]
reverse_list xs = reverse xs

concat_two_lists :: [a] -> [a] -> [a]
concat_two_lists a b = a ++ b 

add_one :: (Num a) => [a] -> [a]
add_one [] = []
add_one (x:xs) = x+1 : add_one xs

holds_69 :: [Int] -> Bool
holds_69 [] = False
holds_69 [x] = False
holds_69 (t:x:xs)
  | t == 6 && x == 9 = True
  | x == 6 = holds_69 (x:xs)
  | otherwise = holds_69 xs

my_even :: [Int] -> [Bool]
my_even [] = []
my_even xs = map even xs

my_map_lambda :: [Int] -> [(Int,Int)]
my_map_lambda [] = []
my_map_lambda xs = map (\x -> (x,x^2)) xs

my_filter_pos :: [Int] -> [Int]
my_filter_pos [] = []
my_filter_pos xs = filter (>=0) xs

my_filter_lambda :: [Int] -> [Int]
my_filter_lambda [] = []
my_filter_lambda xs = filter (\x -> x^2 > 25) xs

-- any, all -- 1st param condition, 2nd param list
-- any odd [2,3,27] = True
-- filter (all (>0)) xs
--
-- foldl, foldr -- 1st p binary op, 2nd value, 3d list
-- foldl (+) 0 [1..3] = ((0+1)+2)+3
-- foldr (+) 0 [1..3] = 1+(2+(3+0))

-- list comprehension (order!)

sum_if_prod_even :: [Int] -> [Int] -> [Int]
sum_if_prod_even [] b = b
sum_if_prod_even a [] = a
sum_if_prod_even l1 l2 = [a+b+c | a <- l1, b <- l2, let c = a*b, even c]
-- generation, local declaration, local condition (Bool)

get_positive :: [Int] -> [Int]
get_positive a = [x | x <- a, x>0]

rm_spaces :: [Char] -> [Char]
rm_spaces [] = []
rm_spaces (' ':xs) = rm_spaces xs
rm_spaces (x:xs) = x : (rm_spaces xs)

-- rm_char "asdsfGGsa" 'G'
rm_char :: [Char] -> Char -> [Char]
rm_char [] _ = []
rm_char (x:xs) c
    | x == c = rm_char xs c
    | otherwise = x : (rm_char xs c)

get_max :: (Ord a) => [a] -> a
get_max [] = error "Empty list"
get_max [x] = x
get_max (x:(y:ys))
    | x > y = get_max (x:ys)
    | otherwise = get_max (y:ys)

get_max_pos :: (Ord a) => [a] -> (a, Integer)
get_max_pos [] = error "Empty list"
get_max_pos xs = get_max_pos_intern xs 0 -- index starting at 0
    where
        z = get_max xs
        get_max_pos_intern (x:xs) n
            | x == z = (x,n)
            | otherwise = get_max_pos_intern xs (n+1)

