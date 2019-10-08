module Lec04 where

import Test.QuickCheck

{-    LECTURE 04 : RECURSIVE FUNCTIONS II -}


-- using an accumulator (fast vs slow reverse)
slowReverse :: [a] -> [a]
slowReverse []     = []
slowReverse (x:xs) = slowReverse xs ++ [x]

--    slowReverse [1,2,3,4]
-- == slowReverse [2,3,4] ++ [1]
-- == (slowReverse [3,4] ++ [2]) ++ [1]
-- == ((slowReverse [4] ++ [3]) ++ [2]) ++ [1]
-- == (((slowReverse [] ++ [4]) ++ [3]) ++ [2]) ++ [1]
-- == ((([] ++ [4]) ++ [3]) ++ [2]) ++ [1]
-- == (([4] ++ [3]) ++ [2]) ++ [1]
-- == ...
-- == [4,3,2,1]

--  fastReverse xs acc == reverse xs ++ acc

--  fastReverse (x:xs) acc == reverse (x:xs) ++ acc
--                         == (reverse xs ++ [x]) ++ acc
--                         == reverse xs ++ ([x] ++ acc)
--                         == reverse xs ++ (x:acc)
--                         == fastReverse xs (x:acc)

fastReverse :: [a] -> [a] -> [a]
fastReverse []     acc = acc
fastReverse (x:xs) acc = fastReverse xs (x:acc)

-- quickcheck property
reverseProp :: [Int] -> Bool
reverseProp xs = fastReverse xs [] == slowReverse xs


-- filtering lists
greater :: Ord a => a -> [a] -> [a]
greater x []     = []
greater x (y:ys) = if y > x then y : greater x ys else greater x ys

lesserOrEqual :: Ord a => a -> [a] -> [a]
lesserOrEqual x [] = []
lesserOrEqual x (y:ys) = if y <= x then y : lesserOrEqual x ys
                         else lesserOrEqual x ys

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p []     = []
-- filter p (y:ys) = if p y then y : filter p ys else filter p ys

isPartition :: Int -> [Int] -> Bool
isPartition x xs =
  length (greater x xs ++ lesserOrEqual x xs) == length xs

-- quickcheck property



-- quicksort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
  where smaller = lesserOrEqual x xs
        bigger  = greater x xs
{-
qsort (x:xs) =
  let smaller = lesserOrEqual x xs
      bigger  = greater x xs
  in qsort smaller ++ [x] ++ qsort bigger
-}


insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) = if x < y then x : y : ys
                  else y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)


-- quickcheck property
qsortIsIsort_prop :: [Int] -> Bool
qsortIsIsort_prop xs = qsort xs == isort xs

--                     qsort [2,1,3]
-- ==         qsort [1] ++ [2] ++ qsort [3]
-- == ([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])


-- treesort
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node l y r)
  | x <= y    = Node (insertBST x l) y r
  | otherwise = Node l y (insertBST x r)

buildTree :: Ord a => [a] -> Tree a -> Tree a
buildTree []     acc = acc
buildTree (x:xs) acc = buildTree xs (insertBST x acc)

flatten :: Tree a -> [a] -> [a]
flatten Leaf         acc = acc
flatten (Node l x r) acc =
  let acc1 = flatten r acc
      acc2 = x:acc1
      acc3 = flatten l acc2
  in acc3

treesort :: Ord a => [a] -> [a]
treesort xs = flatten (buildTree xs Leaf) []

-- quickcheck testing
treesortIsQSort_prop :: [String] -> Bool
treesortIsQSort_prop xs = treesort xs == qsort xs
