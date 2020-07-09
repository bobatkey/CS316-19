{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex2ResitTest where

import Prelude hiding (foldr, foldl)
import Ex2

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 2 : HIGHER-ORDER PROGRAMMING              -}
{-                                                                    -}
{-       * * * RESIT TEST QUESTIONS * * *                             -}
{----------------------------------------------------------------------}

{- Your combined score from the submission and the test will be worth
   30% of the overall marks for the class (so one mark is worth half a
   percent).

   This file contains the test questions. Answer the questions in this
   file, and make sure that both are committed to GitLab before
   arranging a call with the course lecturer (Robert Atkey
   <robert.atkey@strath.ac.uk>) -}

{----------------------------------------------------------------------}
{- PART 1 : FUNCTIONS ON TREES AND LISTS                              -}
{----------------------------------------------------------------------}


{- 2.1.4 Summing lengths

   Use 'sum', 'map', and 'length' to compute the sum of the lengths of
   all the lists in a list of lists. For example:

      sumLengths [[], [], []]    == 0
      sumLengths [[1,2], [3,4]]  == 4
      sumLengths [[1], [2], [3]] == 3
      sumLengths [["no", "matter"],["what"],["is","in","the"],["lists"]] == 7
-}

sumLengths :: [[a]] -> Int
sumLengths = undefined

{- 2 MARKS -}


{- 2.1.5 Flattening trees

   Use 'foldTree' to flatten a tree to list in right-to-left order:

      flatten (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) == [3,2,1]
-}

flatten :: Tree a -> [a]
flatten = undefined

{- 3 MARKS -}


{- 2.1.6 foldl and foldr

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
   and one that compares 'b's and makes a comparator for pairs
   '(a,b)', such that for any two pairs (a1,b1), (a2, b2):

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

{- 2.3.13 (TEST) Scaling. Write a function that scales a picture by a
   given amount. For example, it should be the case that

      circle 100

   gives the same bitmap as

      scale 2 (circle 50)

   Use 'transform' to write your function. -}

scale :: Double -> Picture a -> Picture a
scale factor = undefined

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
      What is your name?                 <-- this is output
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
