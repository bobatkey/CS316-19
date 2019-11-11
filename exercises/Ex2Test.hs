{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex2Test where

import Prelude hiding (foldr, foldl)
import Ex2

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 2 : HIGHER-ORDER PROGRAMMING              -}
{-                                                                    -}
{-       * * * TEST QUESTIONS * * *                                   -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 4pm on Monday 11th
   November.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   30% of the overall marks for the class (so one mark is worth half a
   percent).

   This file contains the test questions, which are worth 35% of
   Exercise 2. Put this file in the same directory as your solutions
   to Ex2. Answer the questions in this file, and make sure that both
   files are committed to GitLab both by the end of the lab session. -}

{----------------------------------------------------------------------}
{- PART 1 : FUNCTIONS ON TREES AND LISTS                              -}
{----------------------------------------------------------------------}

{- 2.1.4 Mapping

   Write a function using 'map' that applies a predicate (a function
   that returns a 'Bool') to every element of a list. For example,

      > applyPredicate (\x -> x > 5) [4,5,6]
      [False, False, True]
-}

applyPredicate :: (a -> Bool) -> [a] -> [Bool]
applyPredicate = undefined

{- 1 MARK -}


{- 2.1.5 Summing lengths

   Use 'foldr' to compute the sum of the lengths of all the lists in a
   list of lists. For example:

      sumLengths [[], [], []]    == 0
      sumLengths [[1,2], [3,4]]  == 4
      sumLengths [[1], [2], [3]] == 3
      sumLengths [["no", "matter"],["what"],["is","in","the"],["lists"]] == 7
-}

sumLengths :: [[a]] -> Int
sumLengths = undefined

{- 2 MARKS -}


{- 2.1.6 Flattening trees

   Use 'foldTree' to flatten a tree to list in left-to-right order: -}

flatten :: Tree a -> [a]
flatten = undefined

{- 2 MARKS -}


{- 2.1.7 foldl and foldr

   'foldl' was defined in the lectures as: -}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

{- In many cases, foldl and foldr give the same answers:

     > foldl (+) 0 [1,2,3]
     6
     > foldr (+) 0 [1,2,3]
     6

   Fill in the following two definitions to make 'foldlResult' and
   'foldrResult' (defined below) give different answers (and not raise
   an error). -}

op :: Int -> Int -> Int
op = undefined

u :: Int
u = undefined

foldlResult :: Int
foldlResult = foldl op u [1..10]

foldrResult :: Int
foldrResult = foldr op u [1..10]

{- 1 MARK -}


{----------------------------------------------------------------------}
{- PART 2 : COMPARISON OPERATORS                                      -}
{----------------------------------------------------------------------}

{- 2.2.4 Ordering pairs

   Write a function that takes two comparators, one that compares 'a's
   and one hat compares 'b's and makes a comparator for pairs '(a,b)',
   such that for any two pairs (a1,b1), (a2, b2):

     - if a1 < a2 then (a1,b1) < (a2,b2)
     - if a1 > a2 then (a1,b1) > (a2,b2)
     - if a1 = a2 then (a1,b1) and (a2,b2) are ordered however b1, b2 are

   (this is called "lexicographic ordering", or "dictionary ordering")

   For example:

        > (pair compare compare) (1,2) (1,3)
        LT
        > (pair compare compare) (2,2) (1,3)
        GT
        > (pair (invert compare) compare) (1,2) (1,3)
        LT
        > (pair (invert compare) compare) (2,2) (1,3)
        LT

   Hint: use a 'case'. -}

pair :: Comparator a -> Comparator b -> Comparator (a,b)
pair = undefined

{- 3 MARKS -}


{----------------------------------------------------------------------}
{- PART 3 : PICTURES                                                  -}
{----------------------------------------------------------------------}

{- 2.3.13 Flipping Pictures

   Write functions that flip a 'Picture'. The first function should
   flip top to bottom (and bottom to top). The second should flip left
   to right (and right to left).  Use 'transform' to write your
   functions. -}

flipTopBottom :: Picture a -> Picture a
flipTopBottom = undefined

flipLeftRight :: Picture a -> Picture a
flipLeftRight = undefined

{- 2 MARKS -}


{- 2.3.14 Interpreting Boolean operations on Pictures

   Here is a type of Boolean Picture Expressions, parameterised by the
   type of the 'variables': -}

data BPExp a
  = Var a
  | And (BPExp a) (BPExp a)
  | Or  (BPExp a) (BPExp a)
  | Not (BPExp a)
  deriving Show

{- Write a function that uses your 'pictureAND', 'pictureOR', and
   'pictureNOT' functions to evaluate a 'BPExp a', and a function of
   type 'a -> Picture Bool' that describes how to interpret each 'a'
   as a 'Picture Bool', to a 'Picture Bool': -}

evalBPExp :: BPExp a -> (a -> Picture Bool) -> Picture Bool
evalBPExp = undefined

{- Hint: the see tutorial questions on Higher Order Functions. -}

{- 3 MARKS -}



{----------------------------------------------------------------------}
{- PART 4 : PROCESSES                                                 -}
{----------------------------------------------------------------------}

{- 2.4.5 A chat bot

   Write a value of type 'Process String ()' that when run through
   'runIO' produces the following interaction:

      > runIO chatBot
      Hello! What is your name?          <-- this is output
      Haskell                            <-- this is what the user types
      Hello Haskell!                     <-- this is output

   If the user enters a different name, then the final line should use
   that name instead of 'Haskell'. -}

chatBot :: Process String ()
chatBot = undefined

{- 2 MARKS -}


{- 3.4.8 Testing Equality of Processes.

   Let's say that we want to write a function that tests two processes
   for equality -- that is they do the same input and output
   operations, and end with the same values, for all possible
   inputs. We want to write a function of type:

      eqProcess :: Process x () -> Process x () -> Bool

   Such a function would be very helpful for debugging processes. For
   example, we could discover that there is more than one way of
   writing the same process:

      eqProcess echo (input `sequ` output) == True

   Unfortunately, it is not possible in general to write such a
   function that compares for equality. Roughly speaking, this is
   because if the type of inputs 'x' has infinitely many possible
   values, there is no way to test that both processes handle all
   infinitely many values in the same way.

   However, if we restrict the possible values that are input and
   output by the process to be a type with only finitely many values,
   for example 'Bool' only has the values 'True' and 'False', then we
   can decide equivalence between two processes.

   Complete the missing cases (the bits that are 'undefined') in the
   following function definition to write a function that decides
   equality of two processes. -}

eqProcess :: Process Bool () -> Process Bool () -> Bool
eqProcess (End ())       (End ()) =
  undefined  -- are these processes equal?
eqProcess (Output b1 k1) (Output b2 k2) =
  undefined
  -- do they output the same thing, and do they
  -- do the same thing after that?
eqProcess (Input k1)     (Input k2) =
  undefined
  -- what are the possible input values of type Bool?
eqProcess _ _ =
  False
  -- if they don't agree on 'End'ing, 'Input'ing or
  -- 'Output'ing, then they aren't equal


{- Try it on the 'echo' process. The following example is one that
   should return 'False':

       eqProcess echo (input `sequ` (\_ -> End ())) == False

   While the rewrite of 'echo' ought to return 'True':

       eqProcess echo (input `sequ` output) == True
-}

{- 5 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE  (TEST QUESTIONS)                                  -}
{----------------------------------------------------------------------}
