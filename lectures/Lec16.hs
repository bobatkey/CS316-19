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

{-     LECTURE 16 : FUNCTORS, APPLICATIVE FUNCTORS, and MONADS

   FIXME: introduction: abstracting patterns of data dependency
   between actions. Leading up to being able to execute stuff in
   parallel if need be (Haxl).

-}


{-     Part I : Sequences of Actions

-}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

{- A common pattern, executing a sequence of actions and then applying a
   function at the end. For example, traversing a list, performing
   some action for every element is performed by the 'mapM' function
   we saw in Lecture 12: -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] =
  return []
mapM f (x:xs) =
  do x'  <- f x
     xs' <- mapM f xs
     return (x':xs')

{- The analogous function for traversing 'Tree's, which I'll call
   'mapMTree', looks similar: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

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

   We often have a a sequence of actions to perform, none of which
   depend on the results of the previous action. The final result is
   returned by combining the 'a1', 'a2', 'a3', ... with some function
   'f'.

   Another example of this pattern occurring is with parser
   combinators. For example, to parse an JSON object field
   ('"fieldname" : <value>') we wrote code like:

       do fieldname <- parseStringLiteral
          _         <- parseLiteralChar ':'
          value     <- parseItem
          return (fieldname, value)

   which again consists of a sequence of actions that do not depend on
   each other's results, and a final operation to put together all the
   results.

   Since we see this pattern over and over again, it looks like it
   might be worth investigating ways of tidying it up. One way of
   doing this is to write a separate function for each number of
   actions we want to perform. Then we don't have to go to the bother
   of naming all the intermediate results, and can just use normal
   function application.

   When we have no actions, then we are just returning a single value,
   so we can write:

      return x

   When we have one action to perform, we can use a function 'lift1',
   which "lifts" a function of type 'a -> b' to take actions that
   produce 'a's to actions that produce 'b's: -}

lift1 :: Monad m => (a -> b) -> m a -> m b
lift1 f action1 =
  do a <- action1
     return (f a)

{- When we have two actions, we could write 'lift2': -}

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f action1 action2 =
  do a <- action1
     b <- action2
     return (f a b)

{- When we have three actions, we could write 'lift3': -}

lift3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f action1 action2 action3 =
  do a <- action1
     b <- action2
     c <- action3
     return (f a b c)

{- And so on for 'lift4', 'lift5', ...

   We'd rather not have a special function for each number of
   arguments that we want to lift. Is there a way of defining a small
   number of functions that can be chained together to produce the

   Let's look at the way that function application works in Haskell in
   more detail. Back in Lecture 05, we learned that multiargument
   functions in Haskell are actually single argument functions that
   take one argument and then return a function expecting the rest of
   the arguments. For example, if we have a function with the
   following type:

      addThreeNumbers :: Int -> Int -> Int -> Int

   and we apply it to one number, then we get back a function that
   takes two 'Int's and returns an 'Int':

      addThreeNumbers 1 :: Int -> Int -> Int

   and again if we apply it to another number, we get back a function
   that expects one 'Int' and returns an 'Int':

      addThreeNumbers 1 10 :: Int -> Int

   We could make this process more explicit if we had a way of marking
   where the function applications were happening. Let's define a
   function that does function application: -}

apply :: (a -> b) -> a -> b
apply f a = f a

{- So 'f `apply` a' is the the same as 'f a'. We can use it to delineate
   where function applications are happening.

   Written out more explicitly using '($)', applying the function
   'addThreeNumbers' to two arguments looks like this:

      (addThreeNumbers `apply` 1) `apply` 10

   Spelling this out, we have:

     a) addThreeNumbers :: Int -> (Int -> (Int -> Int))

     b) addThreeNumbers `apply` 1 :: Int -> (Int -> Int)

     c) (addThreeNumbers `apply` 1) `apply` 10 :: Int -> Int

   So, 'apply' takes a pure function 'a -> b' and a pure value 'a' and
   returns something of type 'b'.

   Returning to our original problem, we want to lift function
   application up to actions in some monad. Could we do this if we had
   something similar to 'apply', but that worked on functions and
   arguments that were the result of actions. Let's have a go, and
   call that function 'mapply': -}

mapply :: Monad m => m (a -> b) -> m a -> m b
mapply mf ma =
  do f <- mf
     a <- ma
     return (f a)

{- So 'mapply' takes an action that will return a function, and an
   action that will return a value, and returns an action that runs
   the first two in sequence and then applies the function to the
   value.

   The difference between this function and the 'lift' functions we
   wrote before is that it 

-}

-- Now we don't have to write all the 'lift' functions
-- individually. We can write the original functions like this:

mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f [] =
  return []
mapM_v2 f (x:xs) =
  return (:) `mapply` f x `mapply` mapM_v2 f xs

mapMTree_v2 :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapMTree_v2 f Leaf =
  return Leaf
mapMTree_v2 f (Node l x r) =
  return Node `mapply` mapMTree_v2 f l `mapply` f x `mapply` mapMTree_v2 f r

{- Idea: it looks the same as the normal map, except that we have to put
   in some extra noise to handle the stuff happening behind the scenes. -}


{-   Part III : Applicative, a New Typeclass

   FIXME: so called, because we are doing 'applicative' programming.

      http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
-}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Useful for parsing

-- Every Applicative is a Functor

-- Every Monad is an Applicative and a Functor

-- If we have a Monad and an Applicative implementation for the same
-- type, then it must be the case that (<*>) "acts like" 'mapply'.



-- Good for writing traversals of structures



{- Not every Applicative is a Monad: Triples

   bit like pictures of a fixed size.
-}

data Triple a = MkTriple a a a

instance Functor Triple where
  fmap f (MkTriple a1 a2 a3) =
    MkTriple (f a1) (f a2) (f a3)

instance Applicative Triple where
  pure :: a -> Triple a
  pure a =
    MkTriple a a a

  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  MkTriple f1 f2 f3 <*> MkTriple a1 a2 a3 =
    MkTriple (f1 a1) (f2 a2) (f3 a3)

instance Monad Triple where
  return a =
    MkTriple a a a

  MkTriple a1 a2 a3 >>= f =
    undefined -- which 'a' do we use?

{- EXERCISE: there is a monad implementation for Triple, but it doesn't
   have the property that the applicative implementation agrees with it. -}



{-   Part IV : Data Dependencies and Parallelism

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

-- Similar to the 'Process' type!

makeRequest :: Request -> Fetch Response
makeRequest request =
  Blocked [request] (\[response] -> Done response)

instance Functor Fetch where
  fmap f (Done a) =
    Done (f a)
  fmap f (Blocked requests k) =
    Blocked requests (\responses -> fmap f (k responses))

instance Monad Fetch where
  return x = Done x

  Done a >>= f =
    f a
  Blocked requests k >>= f =
    Blocked requests (\responses -> k responses >>= f)



-- The applicative version allows requests in parallel.

instance Applicative Fetch where
  pure x = Done x

  Done f               <*> Done a =
    Done (f a)

  Done f               <*> Blocked requests k =
    Blocked requests (\responses -> Done f <*> k responses)

  Blocked requests k   <*> Done a =
    Blocked requests (\responses -> k responses <*> Done a)

  Blocked requests1 k1 <*> Blocked requests2 k2 =
    Blocked (requests1 ++ requests2)
      (\responses ->
         let responses1 = take (length requests1) responses
             responses2 = drop (length requests1) responses
         in
         k1 responses1 <*> k2 responses2)
