{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec10 where

import Prelude hiding (return, Either (..))

{-     LECTURE 10 : EXCEPTIONAL PROGRAMMING

   Haskell is famous for not having "side effects", or being "purely"
   functional. What does this mean? Is it a good thing? Is it a bad
   thing?

   Haskell is based around the idea that programs are functions in a
   mathematical sense. They take some inputs and produce an output,
   and (crucially) nothing else. This means that, if we have a
   function with the following type:

        f : Int -> Int

   then we know, from the type, that 'f' is a function that, when we
   give it an 'Int', we will get back an 'Int' (assuming it
   terminates), and nothing else will happen.

   This is in contrast with a language like Java. The analogous method
   signature in Java for 'f' is:

        int f(int x)

   In Java, we know that 'f' is a method that takes 'int's and returns
   'int's, but it may also do any amount of "side effects", extra
   behaviour beyond what is explicitly listed:

    - 'f' may print things to the screen.
    - 'f' may throw 'Error's.
    - 'f' may open network connections and purchase goods from Amazon,
      hack into nuclear weapons systems, or post cat pictures to
      Facebook.

   We have no way of knowing from the method signature whether the 'f'
   method will do any, all, or none of these things.

   So Haskell and Java have different defaults. By default a Haskell
   function is restricted to doing nothing "on the side". In Java, by
   default a method is allowed to do anything on the side. (Strictly
   speaking this isn't totally true: a Java method signature can
   restrict the 'Exception's that may be thrown by using the 'throws'
   keyword, however a Java method can always throw an object whose
   class is a subclass of java.lang.RuntimeException, which doesn't
   need to be listed in the 'throws' list.)

   Both defaults have their tradeoffs. Java's default is liberating
   because you can do whatever you want, but limiting because you have
   no machine checkable way of making sure that a particular method
   *doesn't* do some side effect, aside from writing it in the
   documentation. Haskell's default is liberating because we can
   always be sure to know that nothing will happen on the side, which
   can help with reasoning about and refactoring programs, but
   limiting because we currently have no way of doing side effects!

   So how do we use side effects in Haskell programs? They surely are
   useful, because they are used all the time in other languages.

   We will build our way to the answer by first seeing how we can
   *simulate* side effects in Haskell, and looking at the common
   patterns that arise. First we look at how to simulate the side
   effect of "possibly throwing an exception". -}


{-     Part I : Simulating Exceptions using 'Maybe'

   We've seen the 'Maybe' type several times so far in this course,
   starting in Lecture 02, as a way of making it explicit when a
   function can possibly fail to return a value. To recap, if a
   function promises to return an 'Int', then it has to return an
   'Int'. If it promises to return a 'Maybe Int', then it can either
   return 'Nothing', or 'Just x', where 'x' is an 'Int'.

   Let's use 'Maybe' to simulate throwing exceptions. We'll think of
   'Nothing' as "throwing an exception", and 'Just x' as "returning
   normally with 'x'".

   To make this idea clearer, let's make two short definitions to give
   'Nothing' and 'Just' slightly different names: -}

failure :: Maybe a
failure = Nothing

return :: a -> Maybe a
return x = Just x

{- So now, instead of writing 'Nothing' we can write 'failure' and
   instead of writing 'Just x', we can write 'return'. (Note that,
   unfortunately, we can only do this in expressions, not in
   patterns. Oh well.)

   Now let's write a function using this. The function 'subtractOne'
   takes an 'Int' and subtracts 1 from it; unless the input is 0 or
   less, in which case it throws an exception: -}

subtractOne :: Int -> Maybe Int
subtractOne x = if x > 0 then
                  return (x-1)
                else
                  failure

{- We can think of this as similar to the Java static method:

      static int subtractOne(int x) {
           if (x > 0)
                   return x-1;
           else
                   throw new IllegalArgumentException​();
      }


   (We'll discuss how to have different sorts of 'failure' below,
   corresponding to the different possible exception classes in Java.)

   A more useful function is one that searches for a 'k'ey in a list
   of 'k'ey/'v'alue pairs. Such a search may fail if the key is not
   there, and succeeds (returns normally) if it does find the key. We
   write this like so: -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure
search k ((k',v'):kvs) =
  if k == k' then
    return v'
  else
    search k kvs

{- Now let's try to use this function to build a larger one. We're going
   to write a function that takes a list of key/value pairs, and a
   tree full of keys, and returns a tree of the same shape, but with
   all the keys replaced with their corresponding values from the
   list.

   Here's the 'Tree' type again: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Now we write the function described above. Since searching for a key
   in a list of key/value pairs may fail, looking up all the keys in a
   'Tree' may fail, so the function can only promise to return a
   'Maybe (Tree v)': -}
lookupAll :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)

{- The actual traversal of the Tree pattern matches on whether the input
   Tree is a 'Leaf' or a 'Node'. In the 'Leaf' case, we return the
   'Leaf', using our 'return' helper: -}
lookupAll kvs Leaf = return Leaf

{- In the 'Node l k r' case, we have to do a lot more work. In essence,
   we want to first look up all the keys in the 'l'eft subtree, look
   up the 'k'ey at this node, look up all the keys in the 'r'ight
   subtree and then put the three results back together into a
   'Node'. However, writing this out gets quite complex because each
   of these operations may fail: there may be missing keys in the left
   subtree, the key at this node may be missing, or there may be
   missing keys in the right subtree. So, after each operation, we
   have to match on whether it succeeded or not. If an operation did
   not succeed (i.e., returned 'Nothing'), then the whole thing
   returns 'Nothing'. If an operation does succeed, then we continue
   to the next operation. We can write this behaviour -- simulating
   what exceptions do -- out in full by using a 'case' for each
   operation: -}
lookupAll kvs (Node l k r) =
  case lookupAll kvs l of
    Nothing -> failure
    Just l' ->
      case search k kvs of
        Nothing -> failure
        Just v  ->
          case lookupAll kvs r of
            Nothing -> failure
            Just r' ->
              return (Node l' v r')

{- The structure is the same for each operation ('lookupAll kvs l',
   'search k kvs', 'lookupAll kvs r'). If we call the operation 'op',
   then each step has the same structure:

      case op of
        Nothing -> failure         -- 'op' failed, return 'failure'
        Just x  -> .. carry on ... -- 'op' succeeded, call the result 'x' and carry on

   With three possibly failing operations like this, the "staircase"
   or "cascade" of 'cases' looks nearly bearable. But what if we had
   four, five, six, or twenty operations to be carried out? Following
   this structure would lead to massively indented code marching off
   the side of the screen. Worse, it is highly repetitive code,
   raising the possibility of tiny mistakes creeping in due to the
   difficulty of seeing the differences between many copies of the
   same bit of code.

   So is there a way of fixing this repetition? The key repeated piece
   of code is the 'case op of ...' snippet above. Can we abstract this
   out into a helper function? Following the idea of abstraction by
   taking concrete pieces of functions and giving them names that we
   saw in Lecture 05, we can do this. The first it to abstract out is
   the operation 'op', which must have type 'Maybe a', for some
   'a'. The second bit is the section marked '... carry on ...'
   above. It is less obvious how to abstract this, but the key is to
   notice that it is a piece of code that (a) must return something of
   type 'Maybe b', in order to match the 'failure' on the other
   branch, and that (b) it has access to an additional 'x'
   parameter. These observations lead to the idea that we can rewrite
   the snippet above to:

       case op of
         Nothing -> failure
         Just x  -> k x

   where 'op' has type 'Maybe a', and 'k' has type 'a -> Maybe b'. We
   can now turn this into a higher order function, capturing the
   pattern of "try 'op', and if that works, run 'k' with the
   result". We'll call the function 'ifOK' since it checks its first
   argument to see if it is OK ("returns normally") and if it is, it
   runs its second argument. -}

ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b
ifOK op k = case op of
              Nothing -> Nothing
              Just a  -> k a

{- (Note: the second argument is called 'k' because it is short for
   "continuation" ('k' instead of 'c' is "traditional"). A
   continuation (of a program) is "what happens next".)

   Let's now see how we can use 'ifOK' to simplify our 'lookupAll'
   function. Here's the function again, but with the old 'case's
   commented out and replaced by the corresponding use of 'ifOK'. The
   first two lines are the same, all the changes are in the second
   case: -}

lookupAll_v2 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v2 kvs Leaf = return Leaf
lookupAll_v2 kvs (Node l k r) =
  ifOK (lookupAll_v2 kvs l) (\l' ->
--  case lookupAll kvs l of
--    Nothing -> failure
--    Just l' ->
  ifOK (search k kvs) (\v ->
--      case search k kvs of
--        Nothing -> failure
--        Just v  ->
  ifOK (lookupAll_v2 kvs r) (\r' ->
--          case lookupAll kvs r of
--            Nothing -> failure
--            Just r' ->
              return (Node l' v r'))))

{- Instead of doing pattern matches and a cascade of 'cases', we are
   capturing the behaviour of exceptions in the function 'ifOK',
   passing in the possibly failing operation each time as the first
   argument, and the continuation (what to do next) as the second
   argument. Removing the commented out code shows what a space
   benefit this has been: -}

lookupAll_v3 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v3 kvs Leaf = return Leaf
lookupAll_v3 kvs (Node l k r) =
  ifOK (lookupAll_v3 kvs l) (\l' ->
  ifOK (search k kvs)       (\v ->
  ifOK (lookupAll_v3 kvs r) (\r' ->
  return (Node l' v r'))))

{- A further 'prettification' is to use 'ifOK' as an infix operator,
   putting it between the operation and the continuation
   arguments. This gives: -}

lookupAll_v4 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v4 kvs Leaf = return Leaf
lookupAll_v4 kvs (Node l k r) =
  lookupAll_v4 kvs l  `ifOK` \l' ->
  search k kvs        `ifOK` \v ->
  lookupAll_v4 kvs r  `ifOK` \r' ->
  return (Node l' v r')

{- We can now read this as a sequence of steps:

     1. Lookup the keys in 'l', if that is OK, call the result l'
     2. Search for 'k' in 'kvs', if that is OK, call the result 'v'
     3. Lookup the keys in 'r', if that is OK, call the result r'
     4. Finally, return the Node made with l', v, and r'.

   With a bit of artistic licence, we could think of 'ifOK' as a kind
   of "semicolon" fitting in between the operations performed by our
   program, telling Haskell how to run one operation before another.

   In fact, the type signature of 'ifOK' demonstrates a very useful
   pattern. Let's look at it again:

      ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b

   The first argument is some operation to perform, which may
   fail. The second argument is some operation to perform, if the
   first argument succeeds.

   This pattern is very general. If we replace 'Maybe' with some 'M',
   we get:

        M a -> (a -> M b) -> M b

   And we think of 'M a' as "perform some side effects, which may
   eventually result in a value of type 'a'", then this type expresses
   what we need to define a sequencing operation.

   In the next lecture, we'll see another example of this idea, but
   with stateful computations (i.e., using mutable variables) instead
   of exceptions. In Lecture 12, we'll give this pattern a name:
   Monad. -}


{-      Part II : Catching Exceptions

   The 'ifOK' function captures one aspect of programming with
   exceptions: that if we execute two possibly failing programs in
   sequence, and the first one fails, then everything fails. But, of
   course, it is also possible to catch exceptions and handle
   them. Can we simulate this in Haskell?

   Because we are simulating exceptions with values of type 'Maybe a',
   we can simulate the effect of catching an exception by pattern
   matching on the result of a failable operation. If the operation
   fails, then we do the "handler" code, otherwise we return the
   result.

   Let's see this in action with a simple wrapper for 'search' that
   uses a default value if the searched for key is not in the
   key/value list: -}

searchWithDefault :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault k v kvs =
  case search k kvs of
    Nothing -> return v
    Just a  -> Just a

{- So we run 'search k kvs' to search for 'k' in 'kvs'. If this returns
   'Nothing' (i.e., "throws an exception"), then we "handle" that
   eventuality by doing 'return v'. Otherwise, we pass through the
   returned value 'a'.

   Using a 'case' like this works, but it suffers from the same
   problem as the use of 'case' in 'lookupAll' above. If we have lots
   of 'case's like this, it gets hard to work out what the intended
   behaviour is.

   For 'lookupAll' above, we fixed this problem by defining 'ifOK' to
   capture the pattern of "try this, and if it works, do that". Can we
   capture the pattern of "try this, and if it fails, do that"?

   Let's do this by writing a function 'catch', which looks very
   similar to 'ifOK':  -}

catch :: Maybe a -> Maybe a -> Maybe a
catch op h = case op of
               Nothing -> h
               Just a  -> Just a

{- We can now rewrite 'searchWithDefault' using 'catch' to make the
   exception handling behaviour more explicit: -}

searchWithDefault_v2 :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault_v2 k v kvs =
  catch (search k kvs)
        (return v)

{- As above, writing 'catch' in between its arguments can make it easier
   read: -}

searchWithDefault_v3 :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault_v3 k v kvs =
  search k kvs `catch` return v

{- The 'catch' function demonstrates another common pattern that we'll
   see more of: being able to try one operation, and if that fails,
   trying something else. This pattern is less common than Monads (it
   won't apply in the case of mutable state, for example), but we'll
   see it come back in Lecture 14 on Parsing. -}


{-     Part III : Exceptions with more information

   The 'Maybe' type above is useful for demonstrating the idea of
   simulating exceptions, but in practice it isn't very useful
   because, in the case of failure, it just says 'Nothing', and gives
   no clue what the exception was.

   A better type to use is the built-in type 'Either'. I have
   redefined the type here so you can see it: -}

data Either a b
  = Left a
  | Right b
  deriving Show

{- 'Either a b' is either 'Left a' or 'Right b'. We can think of 'Left
   a' as being like 'Just a', and 'Right b' as like 'Nothing', but
   with a value attached.

   EXERCISE:

     Rewrite 'ifOK' and 'catch' above to use 'Either a b' instead of
     'Maybe a'. Optionally, you can change the type signature of
     'catch' to pass the error message into the handler.

     Once you've done this, rewrite 'search' to return 'Either v
     String', with a useful error message when the key is not found. -}
