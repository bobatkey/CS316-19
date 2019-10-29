{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec10 where

{-          CS316 : Functional Programming

     - Where are we?


     - Where are we going?





     - What's working?
       - Some people like the live coding
       - Some like the lecture notes
       - Some found exercise 1 challenging and enjoyable



     - What isn't?
       - Lecture rooms are terrible (esp. RC471)
       - Exercises are too challenging, and hard
         - Remember that this is *all* coursework
       - Test was not as expected?
       - Tutorials are not useful
         - Too unstructured
         - Timing isn't great (last thing on Thursday)
         - Labs are for working through problems
         - Will publish some questions to work through for this Thursday and the next

       - Haskell isn't used in Industry, why bother studying it?
         - It is used (examples on the GitHub page)
         - Ideas from Haskell are used in other languages
           - ..




     - In general, this is a _programming_ class. To do well, you need
       to write programs, beyond the ones written in the lectures. Try
       taking one of the lecture note sets, and modifying the
       programs. What works? what doesn't?

     - A thing that might help:
       https://personal.cis.strath.ac.uk/robert.atkey/terms.html


     - A Recap:

       *** Part I *** (First order programming and recursion)
       - Lecture 01 : Data and Pattern Matching
       - Lecture 02 : Defining Functions
       - Lecture 03 : Recursive Functions I
       - Lecture 04 : Recursive Functions II

       ===> Exercise 1

       *** Part II *** (Higher order programming)
       - Lecture 05 : Higher order functions
       - Lecture 06 : List comprehensions
       - Lecture 07 : Modelling with datatypes, type classes
       - Lecture 08 : Recursion schemes

       ===> Exercise 2 (finishes on 11th November)

       *** Part III *** (Helpful abstractions)
       - Lecture 09 : Functors and Containers
       - Lecture 10 : Programming with side effects
       - Lecture 11 : Programming with side effects II
       - Lecture 12 : Monads and Applicatives (Programming with side effects III)
       - Lecture 13 : I/O
       - Lecture 14 : Parser Combinators
       - Lecture 15 : Parser Combinators II
       - Lecture 16 : Traversing Containers

       ===> Exercise 3 (7th November -> 2nd December)
         - "Mini project" (implementing a simple database)
         - Can get by with same level of understanding as in Exercise 2,
           but will be "easier" with the material in lectures 09 to 16).

       *** Part IV *** (Extra stuff)
       - Lecture 17 : Laziness
       - Lecture 18 :  <no lecture>
       - Lecture 19 : Parallelism and Concurrency
       - Lecture 20 : Preview of Agda
-}


{-    LECTURE 10 : Programming with Side Effects

   Haskell is famous for not having "side effects", or being "purely"
   functional. What does this mean? Is it a good thing? Is it a bad
   thing?


   -

   - In this lecture: simulating side effects in Haskell, by modelling
     them using "pure" functions.

   - This is will give us an _interface_ to side effects (which we
     will call 'Monad'). The general idea is to represent a _process_
     that uses side effects to generate a value as a valu itself.

   - In Lecture



-}


{-    Part I : Exceptional Programs -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []           = Nothing
search k ((k',v):kvs) = if k == k' then Just v else search k kvs

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

lookupAll :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll kvs Leaf = Just Leaf
lookupAll kvs (Node l k r) =
  case lookupAll kvs l of
    Nothing -> Nothing
    Just l' ->
      case search k kvs of
        Nothing -> Nothing
        Just v  ->
          case lookupAll kvs r of
            Nothing -> Nothing
            Just r' -> Just (Node l' v r')

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing  f = Nothing
ifJust (Just a) f = f a

lookupAll_v2 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v2 kvs Leaf = Just Leaf
lookupAll_v2 kvs (Node l k r) =
  lookupAll_v2 kvs l `ifJust` \l' ->
  search k kvs       `ifJust` \v ->
  lookupAll_v2 kvs r `ifJust` \r' ->
  Just (Node l' v r')


{-    Part II : Programs with State -}

sumTree :: Tree Int -> Int -> (Int, Tree (Int, Int))
sumTree Leaf s =
  (s, Leaf)
sumTree (Node l x r) s =
  let (s1, l') = sumTree l s
      s2       = s1 + x
      (s3, r') = sumTree r s2
  in (s3, Node l' (s1,x) r')

addAll :: Tree (k,v) -> [(k,v)] -> ([(k,v)], Tree k)
addAll Leaf kvs =
  (kvs, Leaf)
addAll (Node l (k,v) r) kvs =
  let (kvs1, l') = addAll l kvs
      kvs2       = (k,v) : kvs1
      (kvs3, r') = addAll r kvs2
  in (kvs3, Node l' k r')

type State s a = s -> (s,a)

andThen :: State s a -> (a -> State s b) -> State s b
andThen t f = \s -> let (s0, a) = t s
                        (s1, b) = f a s0
                    in (s1, b)




{-    Part III : The common pattern: Monads -}
