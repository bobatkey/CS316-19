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



{-     LECTURE 16 : MONADS, DATA DEPENDENCIES, AND APPLICATIVES

   Since Lecture 12, we've been using the 'Monad' typeclass as a
   general interface for organising side effecting computations. Here
   is the 'Monad' definition again: -}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

{- The 'Monad' type class attempts to capture the idea that values of
   type 'm a' represent side effecting operations that result in
   values of type 'a'. To fulfil the 'Monad' interface, a type
   constructor 'm' must implement a 'return' function, that generates
   a "do nothing" operation, and a bind function '>>=', that sequences
   two operations.

   A key point about the '>>=' function is that it allows for the
   second operation to depend on the value returned by the first. This
   is because a function of type 'a -> m b' can decide what side
   effecting operation it wants to do by looking at the 'a' value. We
   say that there is a potential data dependency between the two
   computations.

   In this lecture, we'll look at an alternative interface to side
   effecting computations that still allows computations to be
   sequenced, but disallows data dependencies between them. This
   interface will be called 'Applicative', after an old name for
   Functional Programming ("Applicative Programming", because it is
   based around the idea of applying functions).

   Applicatives will be useful in two ways. First, it makes some
   programs a bit nicer to write, making their structure more
   clear. Second, disallowing data dependencies means that it more
   obvious when certain operations can be run in parallel. This second
   advantage has been put to use in the Haxl library developed by
   Facebook. We will develop a toy version of Haxl at the end of this
   lecture. -}



{-     Part I : Sequences of Actions

   To introduce the idea of Applicatives, we first look at a common
   pattern when using monads. Often, we will execute a sequence of
   actions, none of which depend on the results of the earlier ones,
   and then we apply a function to all the results at the end. For
   example, traversing a list, performing some action for every
   element is performed by the 'mapM' function we saw in Lecture 12: -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] =
  return []
mapM f (x:xs) =
  do x'  <- f x
     xs' <- mapM f xs
     return (x':xs')

{- The analogous function for traversing 'Tree's, which I'll call
   'mapTreeM', looks similar: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f Leaf =
  return Leaf
mapTreeM f (Node l x r) =
  do l' <- mapTreeM f l
     x' <- f x
     r' <- mapTreeM f r
     return (Node l' x' r')

{- These functions both follow the pattern of:

       do a1 <- action1
          a2 <- action2
          a3 <- action3
          ...
          return (f a1 a2 a3 ...)

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

   (notice how the 'lift1' function has a similar type signature to
   'mapPicture' from exercise 2, and 'lift2' has a similar type
   signature to 'mapPicture2'.)

   We'd rather not have a special function for each number of
   arguments that we want to lift. Is there a way of defining a small
   number of functions that can be chained together to produce the
   same effect?

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
   wrote before is that the function argument is also the result of
   some action. This will allow us to chain together several uses of
   'mapply' to reach any number of arguments.

   (Notice that 'mapply' has a similar type signature to the
   'pictureApply' function from Exercise 2.)

   We can see how 'mapply' works by using it to write the 'lift'
   functions from earlier: -}

lift1_v2 :: Monad m => (a -> b) -> m a -> m b
lift1_v2 f action1 =
  return f `mapply` action1

lift2_v2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2_v2 f action1 action2 =
  return f `mapply` action1 `mapply` action2

lift3_v2 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3_v2 f action1 action2 action3 =
  return f `mapply` action1 `mapply` action2 `mapply` action3

{- Each one works in the same way: it uses 'return' to lift the function
   'f' up into the monad, and then uses 'mapply' to repeatedly apply
   it to arguments. 'mapply' is like a version of function application
   that allows side effects to happen at the same time.

   We can now rewrite 'mapM' and 'mapTreeM' to use 'mapply' instead,
   making their structure a bit more explicit. They both work by
   applying the appropriate constructor ('(:)' for lists, 'Node' for
   trees) to the results of processing the sub-lists/trees and
   data. Using 'mapply' instead of normal function application allows
   the side effects to be processed correctly: -}

mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f [] =
  return []
mapM_v2 f (x:xs) =
  return (:) `mapply` f x `mapply` mapM_v2 f xs

mapTreeM_v2 :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM_v2 f Leaf =
  return Leaf
mapTreeM_v2 f (Node l x r) =
  return Node `mapply` mapTreeM_v2 f l `mapply` f x `mapply` mapTreeM_v2 f r

{- In both cases, the idea is that the function looks the same as the
   normal 'map' / 'mapTree', except that we have to put in some extra
   noise to handle the side effects. -}


{-     Part II : Applicative, a New Typeclass

   Often, when we want to do programming with side effects, it
   suffices to use only 'return' and 'mapply', like in 'mapM' and
   'mapTreeM'. This was first observed by Conor McBride and Ross
   Paterson in their paper "Applicative Programming with Effects":

      http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

   They proposed an interface to side effects that is based around
   only a 'return'-like function and a 'mapply'-like function. The
   names they proposed were 'pure' and '<*>' (pronounced "apply"). The
   type class that puts these two together is called 'Applicative'
   (after "Applicative Programming", as mentioned above): -}

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{- The 'Functor f =>' bit means that every 'Applicative' implementation
   must also have a 'Functor' implementation, where 'Functor' is the
   type class for container-like things that we introduced in Lecture
   09: -}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

{- We can always define an 'fmap' for every 'Applicative', because it is
   the same as the 'lift1' function we saw above. So we could define:

      fmap f action = pure f <*> action

   But the design of the standard library allows us to write a custom
   implementation of 'fmap' if we need to, in case it might be more
   efficient.

   As we have seen with the definition of 'mapply', every 'Monad' is
   an 'Applicative' by defining 'pure' to be 'return' and '<*>' to be
   'mapply'. The official definition of 'Monad' in the standard
   library is not quite like how we defined it above. It requires an
   implementation of 'Applicative', just as 'Applicative' requires an
   implementation of 'Functor':

      class Applicative m => Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

   There is also a convention that if a type constructor has a 'Monad'
   interface, then the 'Applicative' interface should act as if 'pure'
   is the same as 'return', and '<*>' is the same as 'mapply'.

   This convention means that there are useful 'Applicative's that do
   not have matching 'Monad' implementations. For example, the type of
   triples: -}

data Triple a = MkTriple a a a
  deriving Show

{- We can think of a value of 'Triple a' as like the pictures from
   Exercise 2, except with only three points.

   Defining a 'Functor' implementation is a matter of applying a
   single function to every point (like 'mapPicture'): -}

instance Functor Triple where
  fmap f (MkTriple a1 a2 a3) =
    MkTriple (f a1) (f a2) (f a3)

{- Defining an 'Applicative' implementation has two operations: 'pure'
   takes a single value and puts it in every point, and '<*>' takes
   the function at each point and applies to the value at that point: -}

instance Applicative Triple where
  pure :: a -> Triple a
  pure a =
    MkTriple a a a

  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  MkTriple f1 f2 f3 <*> MkTriple a1 a2 a3 =
    MkTriple (f1 a1) (f2 a2) (f3 a3)

{- EXERCISE: there is a possible monad implementation for Triple, but it
      doesn't have the property that the applicative implementation
      agrees with it. Write the monad implementation, and show that
      'mapply' for it doesn't give the same answer as the '<*>'
      defined here. -}


{- We have seen that the 'Applicative' interface is all we need to
   define functions that perform traversals of data structures like
   lists and trees. Writing the same functions using the 'Applicative'
   interface is even more concise: -}

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (x:xs) = pure (:) <*> f x <*> mapA f xs

mapTreeA :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
mapTreeA f Leaf         = pure Leaf
mapTreeA f (Node l x r) = pure Node <*> mapTreeA f l <*> f x <*> mapTreeA f r

{- Also, these functions now work for any 'Applicative', not just any
   'Monad'. As we saw with the 'Triple' example, there are potentially
   useful types that are 'Applicative', but not 'Monad'. The 'Picture'
   type from Exercise 2 is another example.

   'Applicatives' are also useful for parsing. The example parser we
   saw above can now be written as:

       pure (\fieldname _ value -> (fieldname, value))
          <*> parseStringLiteral
          <*> parseLiteralChar ':'
          <*> parseItem

   which makes the separation of the "things to be recognised" and the
   "way of combining them" more clear.

   It is also possible to give an alternative implementation of the
   'Parser' type from Lecture 14 that exploits the denial of data
   dependency by the 'Applicative' interface to make parsing more
   efficient by having a fixed grammar that can be optimised. We'll
   not look into this in this course. Instead, we'll look at a way
   that the 'Applicative' interface can be used to make data retrieval
   operations happen in parallel. -}


{-       Part III : Data Dependencies and Parallelism

   As we mentioned above, the key difference between the 'Monad' and
   'Applicative' interfaces is whether or not the second operation to
   be performed can depend on the result of the first. In the 'Monad'
   "bind" function:

      (>>=) :: m a -> (a -> m b) -> m b

   the second operation is a function 'a -> m b', which can inspect
   the 'a' value and return any operation it likes. In the
   'Applicative' "apply" function:

      (<*>) :: f (a -> b) -> f a -> f b

   The two operations are fixed from the start, only the final 'b'
   value can depend on the results of the operations. This lack of
   data dependency can be exploited to reveal opportunities for
   parallelism: if the two operations given to '<*>' can't depend on
   each other, then they could be executed in parallel.

   Facebook's Haxl library is an implementation of this idea. Haxl is
   used within Facebook to implement high performance data fetching
   tasks. Generating a webpage often requires fetching data from a
   multitude of independent backend data services. Performing these
   fetches in parallel can drastically lower the response time of a
   website.

   Haxl's code is available on GitHub:

      https://github.com/facebook/Haxl

   and there is paper describing the idea, evaluating it on some
   benchmarks:

      http://simonmar.github.io/bib/papers/haxl-icfp14.pdf

   The paper is very readable and is at about the level of Haskell
   that you're at in this course. The start of the paper describes a
   toy version of Haxl to get the idea across. Here I'll describe a
   slightly different toy version that is just enough not-toy to be
   able to implement a real concurrent fetching implementation for it
   in Lecture 20.

   For our toy version, we'll just represent requests to and responses
   from backend data services as 'String's: -}

type Request = String
type Response = String

{- The core data structure is 'Fetch', which represents a sequence of
   batches of requests to be made: -}

data Fetch a
  = Done a
  | Blocked [Request] ([Response] -> Fetch a)

{- A 'Fetch a' value is either:

    1. 'Done x', representing a job that has completed and returned
       the value 'x'; or

    2. 'Blocked requests k' representing a job that is blocked on a
       list of 'requests'. Once these requests have been fulfilled,
       the continuation 'k' can be applied to the responses, yielding
       the rest of the job.

   Notice that 'Fetch' is very similar to the 'Process' type in
   Exercise 2. There are two key differences:

    1. The 'Process' type had separate 'Input' and 'Output'
       constructors, which have been merged here -- every 'Output' is
       immediately followed by an 'Input'.

    2. The "outputs" and "inputs" are lists of requests and responses,
       because we want to record which requests can be run in
       parallel.

   To make individual requests, 'Fetch' has a primitive operation
   'makeRequest', which takes a 'Request' and returns a 'Fetch' job
   that will yield a 'Response': -}

makeRequest :: Request -> Fetch Response
makeRequest request =
  Blocked [request] (\[response] -> Done response)

{- To see how 'Fetch' works we'll define a custom 'Show' implementation
   for it. We can't in general print out functions, but we can print
   out the first thing that a 'Fetch' job will do. Either it is
   'Done', or it is 'Blocked' on a list of requests: -}

instance Show a => Show (Fetch a) where
  show (Done a)             = "(Done " ++ show a ++ ")"
  show (Blocked requests _) = "(Blocked " ++ show requests ++ " <waiting>)"

{- For example, 'makeRequest "A"' generates a 'Fetch' job that is
   blocked on the request "A":

      > makeRequest "A"
      (Blocked ["A"] <waiting>)

   We'll now define a 'Monad' implementation for 'Fetch'. The 'return'
   generates 'Fetch' jobs that are already 'Done'. The "bind" ('>>=')
   sequences 'Fetch' jobs one after the other, in a similar way to the
   'sequ' function in Exercise 2 sequences 'Process'es one after the
   other. -}

instance Monad Fetch where
  return :: a -> Fetch a
  return x = Done x

  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  Done a >>= f =
    f a
  Blocked requests k >>= f =
    Blocked requests (\responses -> k responses >>= f)

{- For example, if we write a 'Fetch' job that makes two requests in
   sequence, like this: -}

sequentialJob :: Fetch (Response, Response)
sequentialJob =
  do a <- makeRequest "A"
     b <- makeRequest "B"
     return (a, b)

{- Then if we print this out, we can see that the 'Fetch' job is first
   'Blocked' on the request for "A", and will not proceed until this
   request is fulfilled:

      > sequentialJob
      (Blocked ["A"] <waiting>)

   If we could print out functions, then the whole 'Fetch' job would
   look like this, showing that the "A" request is scheduled to happen
   before the "B" request.

      Blocked ["A"] (\[a] -> Blocked ["B"] (\[b] -> Done (a,b)))

   An implementation of the 'Monad' interface has no option but to
   sequence the jobs, because it cannot access the second 'Fetch' job
   until it knows the value returned by the first, due to the
   potential data dependency.

   In contrast, the 'Applicative' interface has access to both of the
   'Fetch' jobs it is given, and can merge them when it sees fit,
   allowing for multiple requests with no data dependency to be
   parallelised. Here is the implementation of 'Applicative' for
   'Fetch': -}

instance Applicative Fetch where
  pure :: a -> Fetch a
  pure x = Done x
{- The 'pure' implementation is the same as the 'return' in the 'Monad'
   implementation. -}

{- The 'apply' ('<*>') implementation merges the two 'Fetch' jobs: -}
  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Done f               <*> Done a =
    Done (f a)
{- If both are done, then it applies the function from the first to the
   value from the second. -}

  Done f               <*> Blocked requests k =
    Blocked requests (\responses -> Done f <*> k responses)

  Blocked requests k   <*> Done a =
    Blocked requests (\responses -> k responses <*> Done a)
{- If one is done and the other is blocked, then the result is blocked
   on the same requests, and 'apply' is called recursively to apply
   the 'Fetch' job after the response has been received. -}

  Blocked requests1 k1 <*> Blocked requests2 k2 =
    Blocked (requests1 ++ requests2)
      (\responses ->
         let responses1 = take (length requests1) responses
             responses2 = drop (length requests1) responses
         in
         k1 responses1 <*> k2 responses2)
{- If both are Blocked, then we concatenate the lists of requests,
   creating a single 'Blocked' constructor that contains several
   requests that can be executed in parallel. The continuation then
   has to separate the responses and pass them on to the two
   continuations. -}

{- Since every Applicative must also have a 'Functor' implementation, we
   just give the simplest 'fmap' implementation in terms of 'pure' and
   '<*>' as we described above: -}

instance Functor Fetch where
  fmap :: (a -> b) -> Fetch a -> Fetch b
  fmap f fetch = pure f <*> fetch

{- To see the difference this 'Applicative' instance makes, we rewrite
   the 'sequentialJob' 'Fetch' to use the 'Applicative' interface
   instead: -}

parallelJob :: Fetch (Response, Response)
parallelJob =
  pure (\a b -> (a, b)) <*> makeRequest "A" <*> makeRequest "B"

{- If we print this out, we can see that it has merged the two requests
   "A" and "B" into one 'Blocked', meaning that something executing
   these 'Fetch' jobs can executed them in parallel:

       > parallelJob
       (Blocked ["A","B"] <waiting>)

   If we could print out functions, then 'parallelJob' would look
   like this:

       Blocked ["A","B"] (\[a,b] -> Done (a,b))

   Showing that there is only one 'Blocked', with two jobs to run in
   parallel. Once they have both finished, the task is 'Done'.

   In Lecture 20, we will return to 'Fetch' and write a real parallel
   executor that executes the requests in parallel, using the
   concurrency features of Haskell. -}
