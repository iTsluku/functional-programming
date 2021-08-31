module T3 where

-- type
type String = [Char] -- type declaration
type Vec3 = (Int,Int,Int) -- 3dim coord

vector_add :: Vec3 -> Vec3 -> Vec3
vector_add (a,b,c) (d,e,f) = (a+d,b+e,c+f) 

vector :: (Int,Int,Int)
vector =  vector_add (1,2,3) (2,5,7)

vector_2 :: Vec3
vector_2 = vector

-- data

-- Money :: type constructor
-- Euro :: data constructor
-- Dollar :: data constructor
data Money = Euro Double | Dollar Double deriving Show

-- to_dollar (Euro 100) = 118.0
to_dollar :: Money -> Money
to_dollar (Euro a) = Dollar (1.18*a)
to_dollar (Dollar a) = Dollar a

to_euro :: Money -> Money
to_euro (Dollar a) = Euro (0.85*a)
to_euro (Euro a) = Euro a

-- lifting
data Check a = Nope | Yep a deriving Show -- polymorph data type

safe_div :: Double -> Double -> Check Double
safe_div _ 0 = Nope
safe_div a b = Yep (a/b)

data BinTree a = EmptyTree | Node (BinTree a) a (BinTree a) deriving Show

-- my_root (Node (Node EmptyTree 4 EmptyTree) 2 (Node EmptyTree 0 EmptyTree)) = 2
my_root :: BinTree a -> a
my_root EmptyTree = error "Empty Tree"
my_root (Node _ a _) = a

-- empty_tree (Node (Node EmptyTree 4 EmptyTree) 2 (Node EmptyTree 0 EmptyTree)) = False
empty_tree :: BinTree a -> Bool
empty_tree EmptyTree = True
empty_tree _ = False


data MyList a = Empty | Head a | Tail a deriving Show

my_head :: [a] -> MyList a
my_head [] = Empty
my_head (x:xs) = Head x

my_tail :: [a] -> MyList a
my_tail [] = Empty
my_tail [x] = Tail x
my_tail (x:xs) = my_tail xs


