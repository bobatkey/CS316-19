{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
module Lec16 where

import Prelude hiding (Functor (..), Applicative (..), Monad (..), mapM)

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y

{-     LECTURE 16 : MONADS, FUNCTORS, and APPLICATIVES

-}

{-     Part I : Sequences of Actions

-}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

{- The analogous function for traversing 'Tree's, which I'll call
   'mapMTree', looks similar: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] =
  return []
mapM f (x:xs) =
  do x'  <- f x
     xs' <- mapM f xs
     return (x':xs')

mapMTree :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapMTree f Leaf =
  return Leaf
mapMTree f (Node l x r) =
  do l' <- mapMTree f l
     x' <- f x
     r' <- mapMTree f r
     return (Node l' x' r')

{- These functions both follow the pattern of:

       do a1 <- action1
          a2 <- action2
          a3 <- action3
          ...
          return (f a1 a2 a3 ...)

   which is very common.
-}

{-
       do fieldname <- parseStringLiteral
          _         <- parseLiteralChar ':'
          value     <- parseItem
          return (fieldname, value)
-}

lift0 :: Monad m => a -> m a
lift0 x = return x

lift1 :: Monad m => (a -> b) -> m a -> m b
lift1 f action1 =
  do a <- action1
     return (f a)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f action1 action2 =
  do a1 <- action1
     a2 <- action2
     return (f a1 a2)


-- addThreeIntegers :: Int -> (Int -> (Int -> Int))
-- addThreeIntegers 1 ::       Int -> (Int -> Int)
-- addThreeIntegers 1 10 ::            Int -> Int
-- addThreeIntegers 1 10 20 ::                Int

apply :: (a -> b) -> a -> b
apply f a = f a

-- ((addThreeIntegers `apply` 1) `apply` 10) `apply` 20

mapply :: Monad m => m (a -> b) -> m a -> m b
mapply action1 action2 =
  do f <- action1
     a <- action2
     return (f a)


-- mapM_v2
mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f [] =
  return []
mapM_v2 f (x:xs) =
  return (:) `mapply` f x `mapply` mapM_v2 f xs
{-
  do x'  <- f x
     xs' <- mapM f xs
     return (x':xs')
-}


-- mapMTree_v2


{-   Part II : Applicative, a New Typeclass

   FIXME: so called, because we are doing 'applicative' programming.

      http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
-}

class Functor f where
  fmap  ::   (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure  :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b

-- (>>=) :: f a -> (a -> f b) -> f b

-- Useful for parsing
{-
       do fieldname <- parseStringLiteral
          _         <- parseLiteralChar ':'
          value     <- parseItem
          return (fieldname, value)

    pure (\fieldname _ value -> (fieldname, value))
       <*> parseStringLiteral
       <*> parseLiteralChar ':'
       <*> parseItem
-}

-- Every Applicative is a Functor
fmapA :: Applicative f => (a -> b) -> f a -> f b
fmapA g action = pure g <*> action

-- Every Monad is an Applicative and a Functor
--- mapply :: Monad m => m (a -> b) -> m a -> m b

-- If we have a Monad and an Applicative implementation for the same
-- type, then it must be the case that (<*>) "acts like" 'mapply'.

-- Good for writing traversals of structures

{- Not every Applicative is a Monad: Triples

   bit like pictures of a fixed size.
-}

data Triple a = MkTriple a a a

-- Functor, Applicative, not a Monad
instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (MkTriple a1 a2 a3) =
    MkTriple (f a1) (f a2) (f a3)

instance Applicative Triple where
  pure x =
    MkTriple x x x

  MkTriple f1 f2 f3 <*> MkTriple a1 a2 a3 =
    MkTriple (f1 a1) (f2 a2) (f3 a3)

get1 :: Triple a -> a
get1 (MkTriple a _ _) = a

get2 :: Triple a -> a
get2 (MkTriple _ a _) = a

get3 :: Triple a -> a
get3 (MkTriple _ _ a) = a

instance Monad Triple where
  return x = MkTriple x x x

  (>>=) :: Triple a -> (a -> Triple b) -> Triple b
  MkTriple a1 a2 a3 >>= f =
    MkTriple (get1 (f a1)) (get2 (f a2)) (get3 (f a3))

{-   Part III : Data Dependencies and Parallelism

   What use is the Applicative Typeclass?

   Facebook's Haxl:

     - https://github.com/facebook/Haxl
     - http://simonmar.github.io/bib/papers/haxl-icfp14.pdf

   Here follows a toy version of Haxl.

   Basic idea is that we can exploit the lack of data dependencies in
   a sequence like:

      do resp1 <- request1
         resp2 <- request2
         resp3 <- request3
         return (f resp1 resp2 resp3)

   to do 'request1', 'request2', and 'request3' in parallel. -}

type Request = String
type Response = String

data Fetch a
  = Done a
  | Blocked [Request] ([Response] -> Fetch a)

makeRequest :: Request -> Fetch Response
makeRequest request =
  Blocked [request] (\[response] -> Done response)

instance Functor Fetch where
  fmap f (Done a) = Done (f a)
  fmap f (Blocked requests k) =
    Blocked requests (\responses -> fmap f (k responses))

instance Monad Fetch where
  return x = Done x

  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  Done x >>= f = f x
  Blocked requests k >>= f =
    Blocked requests (\responses -> k responses >>= f)

-- makeRequest "A" >>= \a -> makeRequest "B" >>= \b -> return (a,b)
--
--     Blocked ["A"] (\[response] -> Done response)
-- >>= \a -> Blocked ["B"] (\[response] -> Done response)
-- >>= \b -> return (a,b)
--
-- Blocked ["A"] (\[a] -> Blocked "B" (\[b] -> Done (a,b)))

instance Applicative Fetch where
  pure :: a -> Fetch a
  pure x = Done x

  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Done f               <*> Done a = Done (f a)
  Blocked requests k   <*> Done a =
    Blocked requests (\responses -> k responses <*> Done a)
  Done f               <*> Blocked requests k =
    Blocked requests (\responses -> Done f <*> k responses)
  Blocked requests1 k1 <*> Blocked requests2 k2 =
    Blocked (requests1 ++ requests2)
      (\responses ->
         let responses1 = take (length requests1) responses
             responses2 = drop (length requests1) responses
         in k1 responses1 <*> k2 responses2)

-- pure (\a b -> (a,b)) <*> makeRequest "A" <*> makeRequest "B"
--
--  ...
--
-- Blocked ["A","B"] (\[a,b] -> Done (a,b))

-- Blocked ["A"] (\[a] -> Blocked "B" (\[b] -> Done (a,b)))
