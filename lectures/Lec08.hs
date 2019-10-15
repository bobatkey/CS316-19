{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec08 where

import Prelude hiding (sum, foldr, foldl, map, filter)

{-    LECTURE 08 : RECURSION SCHEMES -}

{-   PART I : Folding -}

-- sum
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

-- length
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

-- foldr (common pattern)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

sum' :: [Int] -> Int
sum' = foldr (+) 0

len' :: [a] -> Int
len' = foldr (\x l -> 1 + l) 0

product' :: [Int] -> Int
product' = foldr (*) 1


-- defining map, filter
map :: (a -> b) -> [a] -> [b]
map f = foldr (\a bs -> f a : bs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a as -> if p a then a : as else as) []

mapfilter :: (a -> Maybe b) -> [a] -> [b]
mapfilter f = foldr (\a bs -> case f a of
                                Nothing -> bs
                                Just b  -> b : bs)
                    []

-- other datatypes (trees, maybe)

-- Maybe
foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe n j Nothing  = n
foldMaybe n j (Just x) = j x

-- Example
withDefault :: a -> Maybe a -> a
withDefault x = foldMaybe x (\x -> x)


-- reimplement mapfilter

-- Tree
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf                = l
foldTree l n (Node left x right) = n (foldTree l n left) x (foldtree l n right)

-- Example: sum tree
sumTree :: Tree Int -> Int
sumTree = foldTree 0 (\l x r -> l + x + r)


{-   PART II : FOLD LEFT -}

-- foldl
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

-- example
data Direction = Up | Down | Left | Right
  deriving Show

type Position = (Int, Int)

move :: Position -> Direction -> Position
move (x,y) Up    = (x,y+1)
move (x,y) Down  = (x,y-1)
move (x,y) Left  = (x-1,y)
move (x,y) Right = (x+1,y)

-- "Lifting" from single items to lists of items
moves :: Position -> [Direction] -> Position
moves = foldl move


{-   PART III : AGGREGATION  -}

-- crush / reduce (save to next time?)

reduceList :: Monoid a => [a] -> a
reduceList = foldl (<>) mempty

reduceTree :: Monoid a => Tree a -> a
reduceTree
