{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec08Live where

import Prelude hiding ( sum, foldr, foldl, map, filter
                      , Either (..), Maybe (..))

{-      LECTURE 08 : RECURSION SCHEMES -}

{-      Part I : FOLDING OVER LISTS -}

-- total
total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

append' :: [a] -> [a] -> [a]
append' xs ys = appendHelper xs
  where appendHelper []     = ys
        appendHelper (x:xs) = x : appendHelper xs

-- abstracting total : totalPlus
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = x `f` foldr f a xs

total' :: [Int] -> Int
total' = foldr (+) 0

len' :: [a] -> Int
len' = foldr (\_ l -> l+1) 0

--    foldr (\_ l -> l+1) 0 [1,2,3]
-- = 1 `(\_ l -> l+1)` (foldr (\_ l -> l+1) 0 [2,3])
-- = 3

product' :: [Int] -> Int
product' = foldr (*) 1

append'' :: [a] -> [a] -> [a]
append'' xs ys = foldr (:) ys xs


--mapO
mapO :: (a -> b) -> [a] -> [b]
mapO f []     = []
mapO f (x:xs) = f x : mapO f xs

-- map
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x ys -> f x : ys) []

-- filterO
filterO :: (a -> Bool) -> [a] -> [a]
filterO p []     = []
filterO p (x:xs) =
  if p x then x : filterO p xs else filterO p xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x ys -> if p x then x : ys else ys) []



{-      Part II : FOLDING LEFT -}

{-
          foldr f a [x1,x2,x3]
      ==  f x1 (foldr f a [x2,x3])
      ==  f x1 (f x2 (foldr f a [x3]))
      ==  f x1 (f x2 (f x3 (foldr f a [])))
      ==  f x1 (f x2 (f x3 a))

      ==  x1 `f` (x2 `f` (x3 `f` a))
-}

{- What about:

          ((a `f` x1) `f` x2) `f` x3
-}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

--    foldl (+) 0 [1,2,3]
-- == foldl (+) (0 + 1) [2,3]
-- == foldl (+) ((0 + 1) +2) [3]
-- == foldl (+) (((0 + 1) + 2) +3) []
-- == (((0 + 1) + 2) +3)
-- == 6

data Direction = Up | Down | Left | Right
  deriving Show

type Position = (Int, Int)

move :: Position -> Direction -> Position
move (x,y) Up    = (x,y+1)
move (x,y) Down  = (x,y-1)
move (x,y) Left  = (x-1,y)
move (x,y) Right = (x+1,y)

moves :: Position -> [Direction] -> Position
moves = foldl move


{-       Part III : FOLDS FOR OTHER DATATYPES -}

{-       x1 `f` (x2 `f` (x3 `f` a))

         x1  :  (x2  :  (x3  :  []))
-}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf = l
foldTree l n (Node lt x rt) =
  n (foldTree l n lt) x (foldTree l n rt)

--    Node (Node Leaf 1 Leaf) 4 (Node Leaf 5 Leaf)
--
--    add3 (add3 0    1 0)    4 (add3 0    5 0)
-- == add3 1 4 5
-- == 10

totalTree :: Tree Int -> Int
totalTree = foldTree 0 (\l x r -> l + x + r)


data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Show)

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe n j Nothing  = n
foldMaybe n j (Just a) = j a

withDefault :: a -> Maybe a -> a
withDefault d = foldMaybe d (\x -> x)

data Nat
  = Zero
  | Succ Nat
  deriving Show

-- Succ (Succ (Succ (Succ Zero)))

foldNat :: b -> (b -> b) -> Nat -> b
foldNat z s Zero     = z
foldNat z s (Succ n) = s (foldNat z s n)

add :: Nat -> Nat -> Nat
add x y = foldNat y Succ x
