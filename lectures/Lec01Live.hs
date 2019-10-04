{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec01Live where

import Prelude hiding (Left, Right)

{-    LECTURE 01 : PROGRAMS MADE OF EQUATIONS; RUNNING THEM



   Welcome to the first lecture of the University of Strathclyde's
   "Functional Programming" course!

   Most of the lectures will be delivered by live Haskell coding on
   the projector. We take the final version of the code and annotate
   it with comments after the lecture to help you understand what is
   going on, and to recreate some of the things we said during the
   lecture.

   This first lecture introduces the two main concepts in Haskell:

     1. Defining what data is.

     2. Transforming data by pattern matching.

   We will cover both of these in a lot more depth later in the
   course. This lecture is a first introduction to the concepts. We
   will also introduce the concept of 'datatype' and how it relates to
   these concepts. -}



{-      PART 1 : DEFINING DATA AND VALUES
 -}

data Direction = Up | Down | Left | Right
  deriving Show

whereIsTheCeiling :: Direction
whereIsTheCeiling = Up

whereIsTheFloor = Down

flipVertically :: Direction -> Direction
flipVertically Up    = Down
flipVertically Down  = Up
flipVertically Left  = Left
flipVertically Right = Right

-- data Bool = True | False

isVertical :: Direction -> Bool
isVertical Up   = True
isVertical Down = True
isVertical _    = False

equalDirection :: Direction -> Direction -> Bool
equalDirection Up    Up    = True
equalDirection Down  Down  = True
equalDirection Left  Left  = True
equalDirection Right Right = True
equalDirection _     _     = False


{-      PART 2 : RECURSIVE DATA

   Here is an example of a more complex Haskell 'data' declaration. it
   defines a new type 'List' that contains two constructors 'Nil' and
   'Cons': -}

data List a = Nil | Cons a (List a)
  deriving Show

ex1 :: List Int
ex1 = Cons 2 (Cons 7 (Cons 1 Nil))
-- ex2 :: List ??
-- ex2 = Cons Nil (Cons 1 Nil)
ex3 = Cons Nil (Cons Nil Nil)


ex1' :: [Int]
ex1' = 2 : 7 : 1 : []

ex1'' :: [Int]
ex1'' = [2,7,1]

-- ex2'' :: [??]
--ex2'' = [[], 1]

ex3'' :: [[a]]
ex3'' = [[], []]

{-    PART 2 : TRANSFORMING DATA BY PATTERN MATCHING

   As we say above, we define functions by /pattern matching/. This
   means that every function is a list of patterns of data that it can
   match with, and for each pattern a description of how that data is
   transformed. To define functions that operate on

   Here is an example, which totals up all the elements in a list of
   'Int's: -}

total :: List Int -> Int
total Nil         = 0
total (Cons x xs) = x + total xs


{-
      int total(List<Integer> input)
-}

{-
      total (Cons 1 (Cons 3 Nil))
    =                               by the second rule for total
      1 + total (Cons 3 Nil)
    =                               by the second rule for total
      1 + (3 + total Nil)
    =                               by the first rule for total
      1 + (3 + 0)
    =                               by (built in) arithmetic
      1 + 3
    =                               by (built in) arithmetic
      4
-}




{-
      total (Cons True Nil)
    =                               by the second rule for total
      True + total Nil
    =                               by the first rule for total
      True + 0
    =                               cannot add booleans to Ints!
      << ERROR >>

-}







-- append :: List a -> List a -> List a






{-
     append (Cons "Unicorn" (Cons "Rainbow" Nil)) (Cons "Pegasus" Nil)
   =         { by the first rule }
     Cons "Unicorn" (append (Cons "Rainbow" Nil) (Cons "Pegasus" Nil))
   =         { by the first rule }
     Cons "Unicorn" (Cons "Rainbow" (append Nil (Cons "Pegasus" Nil)))
   =         { by the second rule }
     Cons "Unicorn" (Cons "Rainbow" (Cons "Pegasus" Nil))

 -}
