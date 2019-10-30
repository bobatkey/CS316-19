module Lec11 where

import Prelude hiding (return)

{-     LECTURE 11 : PROGRAMMING WITH MUTABLE STATE

-}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Simulating state by passing the state around explicitly. -}

-- several attempts at writing this function...

numberTree :: Tree a -> Int -> (Int, Tree (a, Int))
numberTree Leaf i = (i, Leaf)
numberTree (Node l x r) i =
  let (i0, l') = numberTree l i
      i1       = i0+1
      (i2, r') = numberTree r i1
  in (i2, Node l' (i0,x) r')

type State a = Int -> (Int, a)

return :: a -> State a
return x s = (s, x)

-- Looks like 'ifOK' from the Maybe example!

andThen :: State a -> (a -> State b) -> State b
andThen t k s =
  let (s0, a) = t s
      (s1, b) = k a s0
  in (s1, b)

getAndIncrement :: State Int
getAndIncrement s = (s+1,s)

numberTree_v2 :: Tree a -> State (Tree (a, Int))
numberTree_v2 Leaf = return Leaf
numberTree_v2 (Node l x r) =
  numberTree_v2 l `andThen` \l' ->
  getAndIncrement `andThen` \i  ->
  numberTree_v2 r `andThen` \r' ->
  return (Node l' (i, x) r')


get :: State Int
get s = (s,s)

put :: Int -> State ()
put s' = \s -> (s',())

numberTree_v3 :: Tree a -> State (Tree (a, Int))
numberTree_v3 Leaf = return Leaf
numberTree_v3 (Node l x r) =
  numberTree_v3 l `andThen` \l' ->
  get             `andThen` \i  ->
  put (i+1)       `andThen` \() ->
  numberTree_v3 r `andThen` \r' ->
  return (Node l' (i, x) r')

getAndIncrement_v2 :: State Int
getAndIncrement_v2 =
  get       `andThen` \i ->
  put (i+1) `andThen` \() ->
  return i

{-     Part II : Programming with Output -}

printAndSum :: Tree Int -> ([String], Int)
printAndSum Leaf = ([], 0)
printAndSum (Node l x r)
