{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec03Live where

import Test.QuickCheck

{-      LECTURE 03 : RECURSIVE FUNCTIONS I -}

-- gcd, as an example of a recursive function

gcd0 :: Int -> Int -> Int
gcd0 x y = if x == y then x
           else if x < y then gcd0 x (y - x)
                else gcd0 (x - y) y

gcd1 :: Int -> Int -> Int
gcd1 x y | x == y    = x
         | x <  y    = gcd1 x (y - x)
         | otherwise = gcd1 (x - y) y

gcdsEqual_prop :: Int -> Int -> Property
gcdsEqual_prop = \x y -> x > 0 && y > 0 ==> gcd0 x y == gcd1 x y

gcdsDivisors1_prop :: Int -> Int -> Property
gcdsDivisors1_prop = \x y -> x > 0 && y > 0 ==> x `mod` gcd1 x y == 0

gcdsDivisors2_prop :: Int -> Int -> Property
gcdsDivisors2_prop = \x y -> x > 0 && y > 0 ==> y `mod` gcd1 x y == 0

--    gcd0 12 16
-- == gcd0 12 4
-- == gcd0 8 4
-- == gcd0 4 4
-- == 4


-- searching a list

search :: Int -> [Int] -> Bool
search y []     = False
search y (x:xs) = x == y || search y xs

-- (||) True  y = True
-- (||) False y = y

search2 :: Eq a => a -> [a] -> Bool
search2 y []     =  False
search2 y (x:xs) = x == y || search2 y xs

-- searching an association list
lookup' :: Eq k => k -> [(k,v)] -> Maybe v
lookup' k []           = Nothing
lookup' k ((k',v):kvs) = if k == k' then Just v
                         else lookup' k kvs

keys :: [(k,v)] -> [k]
keys []          = []
keys ((k,v):kvs) = k : keys kvs

--    keys [("a",1),("b",2)]
-- == "a" : keys  [("b",2)]
-- == "a" : "b" : keys []
-- == "a" : "b" : []
-- == ["a","b"]

maybeToBool :: Maybe a -> Bool
maybeToBool Nothing  = False
maybeToBool (Just _) = True

searchLookup_prop :: String -> [(String,Int)] -> Bool
searchLookup_prop k kvs =
  maybeToBool (lookup' k kvs) == search2 k (keys kvs)


-- insertionsort
insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) = if x < y then x : y : ys
                  else y : insert x ys

-- checking a list for sortedness
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x >= y && isSorted (y:xs)

--    isSorted [1,2,3]
-- == 1 <= 2 && isSorted [2,3]
-- == 1 <= 2 && 2 <= 3 && isSorted [3]
-- == 1 <= 2 && 2 <= 3 && True

insertPreserveSort :: Int -> [Int] -> Bool
insertPreserveSort x xs =
  {-isSorted xs ==>-} isSorted (insert x (isort xs))

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

isort_prop :: [Int] -> Bool
isort_prop xs = isSorted (isort xs)



-- fast reverse, using an accumulator
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

frev :: [a] -> [a] -> [a]
frev []     ys = ys
frev (x:xs) ys = frev xs (x:ys)

-- quickCheck property
rev_prop :: [Int] -> Bool
rev_prop xs = rev xs == frev xs []
