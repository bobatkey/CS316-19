{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec05live where

import Prelude hiding (map, filter, (.), fst)

{-    LECTURE 05 : HIGHER ORDER FUNCTIONS  -}

{-    PART I : FUNCTIONS THAT RETURN FUNCTIONS

   We've already seen many functions that take several arguments. An
   example is 'add', which adds two 'Int's and returns an 'Int': -}

one :: Int
one = 1

add :: Int -> (Int -> Int)
add x y = x + y

addTen :: Int -> Int
addTen = add 10

--    addTen 12
-- == add 10 12
-- == 10 + 12
-- == 22

-- addTen2
addTen2 :: Int -> Int
addTen2 = add 10

-- add with lambdas
add2 :: Int -> Int -> Int
add2 = \x -> \y -> x + y

--    add2 10 12
-- == (\x -> \y -> x + y) 10 12
-- == (\y -> 10 + y) 12
-- == (10 + 12)
-- == 22

add3 :: Int -> Int -> Int
add3 = \x y -> x + y

fst :: (a,b) -> a
fst = \(a,b) -> a




{-     PART II : FUNCTIONS THAT TAKE FUNCTIONS AS INPUT

-}

-- ten
ten :: Int
ten = 5 + 5

-- double
double :: Int -> Int
double x = (+) x x

-- applyCopy
applyCopy :: (a -> a -> b) -> a -> b
applyCopy f x = f x x

-- double2
double2 :: Int -> Int
double2 x = applyCopy (+) x

{-   double2 x
  == applyCopy (+) x
  == (+) x x
  == x + x
  == double x
-}

ten2 :: Int
ten2 = applyCopy (+) 5


quadruple :: Int -> Int
quadruple x = double (double x)

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

quadruple2 :: Int -> Int
quadruple2 = twice double

octtuple :: Int -> Int
octtuple = twice quadruple2

{-    octtuple x
   == twice quadruple2 x
   == quadruple2 (quadruple2 x)
   == twice double (twice double x)
   == twice double (double (double x))
   == double (double (double (double x)))
-}


doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = double x : doubleAll xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

doubleAll2 :: [Int] -> [Int]
doubleAll2 = map double

fsts :: [(a,b)] -> [a]
fsts = map fst

withLengths :: [String] -> [(String, Int)]
withLengths = map (\str -> (str, length str))

greater :: Ord a => a -> [a] -> [a]
greater x []     = []
greater x (y:ys) = if y > x then y : greater x ys else greater x ys

greater' x       = filter (\y -> y > x)
lesserOrEqual' x = filter (\y -> y <= x)

lesserOrEqual :: Ord a => a -> [a] -> [a]
lesserOrEqual x [] = []
lesserOrEqual x (y:ys) = if y <= x then y : lesserOrEqual x ys else lesserOrEqual x ys

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (y:ys) = if p y then y : filter p ys else filter p ys


onlyEvens :: [Int] -> [Int]
onlyEvens = filter (\x -> x `mod` 2 == 0)


-- pipelines

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)


pipeline :: [(String,Int)] -> [Int]
pipeline = map snd . filter (\(s,i) -> s == "CS316")

{- Another example uses 'wc -l' to count the number of lines in the
   output of 'grep':

      grep CS316 registered-students.txt | wc -l

   We can mimic this by using 'length': -}

pipeline2 :: [(String,Int)] -> Int
pipeline2 = length . filter (\(s,i) -> s == "CS316")

{- An example of a longer pipeline is the following, which selects every
   other element from a list: -}

{-
zip []     ys     = []
zip xs     []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
-}

everyOther :: [a] -> [a]
everyOther = map snd . filter (\ (i,x) -> i `mod` 2 == 1) . zip [0..]

{- How does this work? Let's break it down:

     1. First, we pair every element in the input list with its index
        by zipping with the infinite list [0..] (remember how we did
        this in Lecture 03)

     2. Then we filter to get only the element of the list with odd
        index.

     3. Then we map 'snd' over the list to throw away the indexes and
        keep the data.

   Graphically, we can visualise the pipeline as follows, with types
   for the intermediate stages:

       zip               filter (...)              map snd
   [a] ---> [(Int, a)] ----------------> [(Int,a)] --------> [a]

   Unfortunately, 'everyOther' isn't particularly efficient. In any
   list of reasonable size, we'll be generating quite large numbers
   when all we are really interested in is whether or not they are
   odd.

   An alternative strategy is to zip with the infinite list

       [False, True, False, True, False, True, ...]

   This will produce a list like:

       [(False, x1), (True, x2), (False, x3), (True, x4), ...]

   Keeping the elements with 'True' in the first element will yield:

       [(True, x2), (True, x4), (True, x6), ...]

   And mapping 'snd' will give us:

       [x2, x4, x6, ...]

   as we want.

   Happily, the Haskell standard library function 'cycle' can produce
   infinite lists like [False, True, False, True, ...]. Given any
   finite list 'l', 'cycle l' repeats that list over and over again
   forever. We can use 'cycle' to code up this alternative strategy
   for 'everyOther': -}

everyOther2 :: [a] -> [a]
everyOther2 =  map snd . filter fst . zip (cycle [False, True])
