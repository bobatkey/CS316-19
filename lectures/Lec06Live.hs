{-# LANGUAGE ParallelListComp #-}
module Lec06Live where

import Prelude hiding (concat,zip,lookup)
import Data.List.Split (splitOn)
import Data.List (sort)


{-     LECTURE 06 : LIST COMPREHENSIONS -}

{-      PART 1 : COMPREHENSIONS -}

ex1 :: [Int]
ex1 = [2,4,6,8]

ex2 :: [Int]
ex2 = [2,4..]

{-
     { 2 * x | x >= 0, x < 5 }
-}

ex3 :: [Int]
ex3 = [ 2 * x | x <- [0..5] ]

squares :: [Int]
squares = [ x ^ 2 | x <- [0..5] ]

allpairs :: [(Int,Int)]
allpairs = [ (x,y) | x <- [0..5], y <- [4..6] ]

allpairsOtherOrder :: [(Int,Int)]
allpairsOtherOrder = [ (x,y) | y <- [4..6], x <- [0..5] ]

ordpairs :: [(Int,Int)]
ordpairs = [ (x,y)
           | x <- [0..5]
           , y <- [x..6]
           ]

concatLists :: [[a]] -> [a]
concatLists xss = [ x | xs <- xss, x <- xs ]

firsts :: [(a,b)] -> [a]
firsts abs = [ a | (a,_) <- abs ]

firsts' :: [(a,b)] -> [a]
firsts' abs = [ fst ab | ab <- abs ]

map0 :: (a -> b) -> [a] -> [b]
map0 f []     = []
map0 f (x:xs) = f x : map0 f xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [ x | x <- xs, p x ]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime 1 = False
prime n = factors n == [1,n]

primes :: [Int]
primes = [ x | x <- [1..], prime x ]

numberedPrimesWrong :: [(Int,Int)]
numberedPrimesWrong = [ (i, x) | x <- primes, i <- [1..] ]

zip :: [a] -> [b] -> [(a,b)]
zip []     ys     = []
zip xs     []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

numberedPrimes :: [(Int,Int)]
numberedPrimes = [ (i, x) | x <- primes | i <- [1..] ]

{-
   1,1,2,3,5,8,13,21,34,55,89,...
-}

fibs :: [Integer]
fibs = 1:1:[ x + y | x <- fibs | y <- tail fibs ]


{-      PART 2 : DATABASES -}

lookup :: Eq k => k -> [(k,v)] -> [v]
lookup k t = [ v | (k',v) <- t, k == k' ]

--  SELECT v
--    FROM t
--   WHERE t.k = k

type Person = String
type Book = String
type Fee = Integer

type Database = [(Person, Book, Fee)]

exampleDB :: Database
exampleDB = [ ("Alice", "Tintin",       1)
            , ("Anna",  "Little Women", 2)
            , ("Alice", "Asterix",      5)
            , ("Rory",  "Tintin",       0)
            ]

books :: Person -> Database -> [Book]
books person db = [ book | (person',book,_) <- db, person == person' ]

latebooks :: Database -> [(Book,Person)]
latebooks db = [ (book, person)
               | (person, book, fee) <- db
               , fee > 0 ]

               -- SELECT (book, person)
               --   FROM db
               --  WHERE db.fee > 0


{-      PART 3 : LIFE and DEATH in GLASGOW -}
