module Lec16 where

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

mapM :: (a -> m b) -> [a] -> m [b]
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

{- Both these functions have a pattern of:

       do a1 <- action1
          a2 <- action2
          a3 <- action3
          ...
          return (f a1 a2 a3 ...)

   which is very common. There is a sequence of actions to perform,
   none of which depend on the results of the previous action. The
   final result is returned by combining the 'a1', 'a2', 'a3',
   ... with some function 'f'.

   Another example of this pattern occurring is with parser
   combinators. For example, to parse an JSON object field
   ('"fieldname" : <value>') we wrote code like:

       do fieldname <- parseStringLiteral
          parseLiteralChar ':'
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

lift1 :: (a -> b) -> m a -> m b
lift1 f m =
  do a <- m
     return (f a)

{- When we have two actions, we could write 'lift2': -}

lift2 :: (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb =
  do a <- ma
     b <- mb
     return (f a b)

{- When we have three actions, we could write 'lift3': -}

lift3 :: (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f ma mb mc =
  do a <- ma
     b <- mb
     c <- mc
     return (f a b c)

{- And so on...

   

-}


-- 2. A useful combinator: 'ap', a bit like 'pictureApply'

ap :: m (a -> b) -> m a -> m b
ap mf ma =
  do f <- mf
     a <- ma
     return (f a)

-- Now we don't have to write all the functions individually. We can
-- write the original functions like this:

mapM_v2 ::

  

lift1_v2 :: (a -> b) -> m a -> m b
lift1_v2 f ma = return f `ap` ma

lift2_v2 :: (a -> b -> c) -> m a -> m b -> m c
lift2_v2 f ma mb = return f `ap` ma `ap` mb

lift3_v2 :: (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3_v2 f ma mb mc = return f `ap` ma `ap` mb `ap` mc

{-   Part III : Applicative, a New Typeclass

   FIXME: so called, because we are doing 'applicative' programming.

-}

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b


class Functor f where
  fmap :: (a -> b) -> f a -> f b

{- Not every Applicative is a Monad -}


{-   Part IV : Data Dependencies and Parallelism

   What use is Applicative
-}




