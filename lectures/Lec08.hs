{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec08 where

import Prelude hiding (sum, foldr, foldl, map, filter, Either (..), Maybe (..))

{-      LECTURE 08 : RECURSION SCHEMES

   In Lecture 05 we looked at some ways that higher order functions
   can be used to combine common patterns of behaviour into single
   functions that are specialised by supplying the right functions to
   them. Our main examples were abstracting the idea of applying a
   function to every element of a list to get a new list ('map') and
   filtering a list based on some condition ('filter').

   In this lecture, we look at some more examples of higher order
   functions that capture common patterns. This time, we look at the
   common pattern of recursing over values of a data type. These
   patterns have the general name of "recursion schemes".

   Recursion schemes capture the pattern of "doing something" to every
   node in a datastructure. They are very similar to the idea of the
   Visitor pattern in OO languages. -}

{-        Part I : FOLDING OVER LISTS

   Most of the functions we have written so far in this course have
   been recursive, because this is the way that Haskell deals with
   data of arbitrary size. In Lecture 01, we saw the 'total' function,
   which sums up the integers in a list of integers: -}

total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

{- Another function we've seen, or used, a few times is 'len', which
   computes the length of a list, again using recursion: -}

len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

{- Looking at these two definitions, we can see that they are quite
   similar. They both do something with the empty list (they both
   return '0'), and they compute their value for a list with head 'x'
   and tail 'xs' by computing a value from the tail and then doing
   something with that value. For 'total' it is added to 'x'; for
   'len', '1' is added to it.

   Another example of a similar function, albeit in a slightly
   obfuscated form, is 'append': -}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

{- The value 'ys' doesn't change throughout the execution of this
   function, so we can rewrite it using a 'where' clause, like so to
   separate out arguments that are changed between calls to 'append'
   and those that are fixed: -}

append' :: [a] -> [a] -> [a]
append' xs ys = appendHelper xs
  where appendHelper []     = ys
        appendHelper (x:xs) = x : appendHelper xs

{- Now we can see that 'appendHelper' looks very much like the 'total'
   and 'len' functions above: there is a thing to do in the '[]' case
   (return 'ys'), and there is a thing to do in the 'x:xs' case
   that is defined in terms of the result of processing 'xs'.

   Now we take the idea from Lecture 05 replacing the specific parts
   of these functions with parameters, to make a generic version of
   all these functions that can be specialised to each one of them.

   Let's start with 'total', as it is perhaps the simplest one. If we
   first make a generic version by turning the specific '0' into an
   argument, then we get a new function that effectively computes the
   sum of a list, plus some initial value: -}

totalPlus :: Int -> [Int] -> Int
totalPlus a []     = a
totalPlus a (x:xs) = x + totalPlus a xs

{- Now we go another step, and generalise from _adding_ the head on to
   the result of processing the rest of the list to performing some
   arbitrary operation that we take as a parameter 'f'. The resulting
   function is called 'foldr' in the Haskell library (short for "fold
   right"): -}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

{- We can now recover 'total' by specialising with the operation being
   '+' and the initial value being '0': -}

total' :: [Int] -> Int
total' xs = foldr (+) 0 xs

{- And we can recover 'len' by specialising with the operation being
   "add one" (ignoring the actual value of 'x') and the initial value
   being '0' again: -}

len' :: [a] -> Int
len' xs = foldr (\x l -> 1 + l) 0 xs

{- We can now quickly write new recursive functions on lists, simply by
   saying what the operation is, and what to do for the empty
   list. For example, computing the product of a list (multiplying all
   the elements) by saying that the operation is '*' and the initial
   value is '1'. -}

product' :: [Int] -> Int
product' xs = foldr (*) 1 xs

{- Similarly, we can rewrite 'append', where the operation we do at
   every stage is '(:)' ("cons"), and the initial value is the second
   list 'ys'. -}

append'' :: [a] -> [a] -> [a]
append'' xs ys = foldr (:) ys xs

{- 'foldr' is surprisingly powerful. In fact, it is possile to write all
   structurally recursive functions on lists (recursive functions that
   only make recursive calls on sublists of the input list) using
   foldr. We won't prove this here, but we can write some functions on
   lists we've seen already using 'foldr'.

   First up, let's rewrite the 'map' function from Lecture 05 using
   'foldr'. The original 'map' looks like: -}

mapO :: (a -> b) -> [a] -> [b]
mapO f []     = []
mapO f (x:xs) = f x : mapO f xs

{- Looking at this function, we can see that it has the same structure
   as 'sum' and 'len' above: a particular value is returned for the
   '[]' case, and the 'x:xs' case is handled by combining 'x' and the
   _result_ of processing 'xs'. So we can write 'map' as a 'foldr': -}

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a bs -> f a : bs) []

{- Similarly, the original 'filter' is written as: -}

filterO :: (a -> Bool) -> [a] -> [a]
filterO p []     = []
filterO p (x:xs) = if p x then x : filter p xs else filter p xs

{- Which translates into a 'filter' as: -}

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a filtered -> if p a then a : filtered else filtered) []

{- EXERCISE: the following recursive function returns the list it is
   given as input: -}

listIdentity :: [a] -> [a]
listIdentity []     = []
listIdentity (x:xs) = x : listIdentity xs

{- Write this function as a 'foldr': -}

listIdentity' :: [a] -> [a]
listIdentity' = foldr undefined undefined

{- EXERCISE: the following recursive function does a map and a filter at
   the same time. If the function argument sends an element to
   'Nothing' it is discarded, and if it sends it to 'Just b' then 'b'
   is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr': -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f xs = foldr undefined undefined xs


{-      Part II : FOLDING LEFT

   Above, I said that the name 'foldr' is short for "fold right". What
   is "fold"ing? and why "right"?

   The answer lies in writing out what 'foldr' does on a general
   list. A call to 'foldr f a [x1,x2,x3]' expands like so:

          foldr f a [x1,x2,x3]
      ==  f x1 (foldr f a [x2,x3])
      ==  f x1 (f x2 (foldr f a [x3]))
      ==  f x1 (f x2 (f x3 (foldr f a [])))
      ==  f x1 (f x2 (f x3 a))

   We say that this is a fold of the list [x1,x2,x3] in the sense that
   it "folds" up the elements of the list using 'f'. The "right" comes
   from the fact that the bracketing is "to the right". This is
   perhaps clearer if we write the 'f's infix:

      ==  x1 `f` (x2 `f` (x3 `f` a))

   This inspires the question "what about bracketing this other way"?
   Is there a function that produces the bracketing:

          ((a `f` x1) `f` x2) `f` x3

   (Note that we've had to put the initial 'a' at the start now,
   instead of at the end.)

   There is! And it is called 'foldl' for "fold left". Let's look at
   the definition: -}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

{- Just as for 'foldr', we take an initial value and an operation. The
   difference now is that instead of doing the operation on the
   element 'x' and the result of processing 'xs', we do the operation
   on the 'a' and 'x', passing the updated copy to the next step. So
   the second argument acts as an accumulator, building up a value,
   starting with 'a' and adding 'x' on to it with each step.

   Writing out 'foldl' on an example list gives us:

         foldl f a [x1,x2,x3]
     ==  foldl f (f a x1) [x2,x3]
     ==  foldl f (f (f a x1) x2) [x3]
     ==  foldl f (f (f (f a x1) x2) x3) []
     ==  (f (f (f a x1) x2) x3)

   and writing the last line out with 'f' infix gives:

     ==  ((a `f` x1) `f` x2) `f` x3

   as expected.

   'foldl' captures the pattern of iterating through a list, updating
   some piece of state as we go. As an illustrative example, let's
   take the 'Direction' data type from Lecture 01, and the Position
   type from Lecture 07: -}

data Direction = Up | Down | Left | Right
  deriving Show

type Position = (Int, Int)

{- And define a 'move' function that takes a 'Position' and a
   'Direction' and moves the 'Position' by the 'Direction': -}

move :: Position -> Direction -> Position
move (x,y) Up    = (x,y+1)
move (x,y) Down  = (x,y-1)
move (x,y) Left  = (x-1,y)
move (x,y) Right = (x+1,y)

{- 'foldl' then "lifts" the single step 'move' function to working on
   lists of 'Direction's: -}

moves :: Position -> [Direction] -> Position
moves = foldl move

{- Here's an example: -}

steps :: [Direction]
steps = [Up, Left, Down]

{- Running 'moves (0,0) steps' will yield (-1,0) since we went:

      (0,0) --Up--> (0,1) --Left--> (-1,1) --Down--> (-1,0)

-}


{-       Part III : FOLDS FOR OTHER DATATYPES

   In Parts I and II, we've looked at folding over lists, from the
   right ('foldr') and from the left ('foldl'). The basic idea we
   started with when creating 'foldr' was to observe that recursive
   functions on lists often fit into the following pattern: they have
   a data value for the empty list, and an operation that combines the
   head of a list with the result of processing the tail. The two bits
   correspond to the two constructors for lists: '[]' and '(:)'
   ("cons").

   This correspondence between the element and the operation and the
   constructors of lists becomes apparent if we remember what 'foldr'
   did on the list '[x1,x2,x3]' we wrote out above, we get:

      x1 `f` (x2 `f` (x3 `f` a))

   compare this to how the list '[x1,x2,x3]' is constructed in terms of
   '[]' and '(:)':

      x1  :  (x2  :  (x3  :  []))

   Running 'foldr f a' on the list '[x1,x2,x3]' has had the effect of
   replacing all the '(:)'s in the list with 'f's, and the final '[]'
   with 'a'.

   Once we have this observation, we can apply it to any datatype
   definition in Haskell, to get a 'fold' operation. The 'fold' for a
   datatype 'X' will have an argument for each of 'X's constructors,
   telling the 'fold' what to do with that constructor. For example,
   here is the 'Tree' datatype again: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- 'Tree a' has two constructors, like lists, but this time the second
   constructor takes three arguments: the left subtree, the data, and
   the right subtree.

   Just as 'foldr' took as arguments what to do for '[]' and what to
   do for '(:)', 'foldTree' takes as arguments what to do for 'Leaf'
   and what to do for 'Node'. The major difference is that the
   operation for 'Node' now takes three arguments (because 'Node'
   takes tree arguments):

      1. The _result_ of processing the left subtree
      2. The data value stored at this node
      3. The _result_ of processing the right subtree

   and it returns the result of processing the current node.

   The definition of 'foldTree' is very similar to 'foldr': -}

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf                = l
foldTree l n (Node left x right) = n (foldTree l n left) x (foldTree l n right)

{- As an example, just as we used 'foldr' to write 'total' for lists, we
   can use 'foldTree' to sum up the values stored in a 'Tree Int', by
   using '0' for the leaves, and adding up the three values for the
   nodes: -}

totalTree :: Tree Int -> Int
totalTree = foldTree 0 (\l x r -> l + x + r)

{- As another example, here is the 'Maybe' type we've seen several
   times: -}

data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Show)

{- 'Maybe' isn't recursive, as lists and trees are, but we can still do
   the same constructor driven approach to making a general "recursion
   scheme" for it.

   From the constructors, 'foldMaybe' takes a value to use when the
   input is 'Nothing' and a function to apply to the 'x' in 'Just x': -}

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe n j Nothing  = n
foldMaybe n j (Just x) = j x

{- (This function is in the standard library under the name 'maybe')

   As an example of using this, here is the 'fromMaybe' function from
   the standard library module 'Data.Maybe', which takes a "default
   value" and a Maybe value. The default value is used when the Maybe
   value is Nothing: -}

fromMaybe :: a -> Maybe a -> a
fromMaybe x = foldMaybe x (\x -> x)

{- EXERCISE: The following is a datatype of Natural Numbers (whole
   numbers greater than or equal to zero), represented in unary. A
   natural number 'n' is represented as 'n' applications of 'Succ' to
   'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe as
   above, work out the type and implementation of a 'fold' function
   for 'Nat's. -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}
