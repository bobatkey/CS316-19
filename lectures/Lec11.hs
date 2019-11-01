module Lec11 where

{-      LECTURE 11 : PROGRAMMING WITH SIDE EFFECTS

   In the last lecture, we saw how to simulate exceptions in Haskell
   by using the 'Maybe' type. However, writing out programs that use
   exceptions in this way was tedious due to the constant use of
   'case' to describe how exceptions behave.

   We found that it was possible to simplify programs that use
   simulated exceptions by defining a helper function, called 'ifOK',
   with the type:

      ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b

   The idea behind 'ifOK' is that the first argument represents the
   result of some operation that may succeed or fail. If it succeeds,
   then the result of the operation is used to run the second
   operation (the "continuation", a function waiting on result of the
   first). The final result is either 'Nothing' if either operation
   fails, or 'Just x' if both operations succeed.

   We saw that 'ifOK' was useful for tidying up the definition of
   programs that are best expressed in terms of exceptions, instead of
   purely functionally. What about other forms of side effect, such as
   mutable state, or printing. Can they be tidied up in the same way?
   Let's find out... -}


{-     Part I : Programming with Mutable State

   First, we'll look at simulating mutable state in Haskell. Mutable
   state is the kind of variables that you are used to in a language
   like Java. Variables in Java can be mutated by setting them to new
   values. For example,

      int x = 5;
       .. some code where x is 5 ..
      x = 10;
       .. some code where x is 10 ..
      while (<blah>) {
         x += 1;
         .. x gets 1 bigger every time the loop executes ..
      }
      .. x could be some number bigger than 10 ..

   In Haskell, variables are immutable. Once they are given a value,
   they keep this value forever.

   We can simulate the behaviour of mutable variables using immutable
   variables by making a new variable each time we would have done a
   mutation. If we are careful to only use the most recently created
   variable, then we can simulate mutable state with immutable state.

   Let's see this idea in action on a simple example. We'll use the
   datatype of trees again: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- The example problem we're going to try to solve this time is how to
   go through a tree, numbering each node from left to right. For
   example, we want to take the tree:

      Node (Node Leaf "A"     Leaf) "B"     (Node Leaf "C"     Leaf)

   and number it like so:

      Node (Node Leaf ("A",0) Leaf) ("B",1) (Node Leaf ("C",2) Leaf)

   If we think about how we might do this in a language with mutable
   state, we might have a variable that keeps track of which number we
   have counted up to which we increment every time we visit a node.

   In Haskell, we don't have mutable state by default, so we'll have
   to simulate it. The simulation works by passing the value of the
   state around as an additional parameter that is passed into
   functions and out again.

   We will solve the numbering problem by writing a function that
   passes over the tree from left to right, taking the current value
   of the counter as an argument, and returning the updated value of
   the counter as an additional result.

   Here is the function: -}

numberTree :: Tree a -> Int -> (Int, Tree (a, Int))
{- The type indicates that (a) it takes a 'Tree a' as input, and also an
   'Int' representing the current value of the counter; and (b) that
   it returns a pair of the new value of the counter, and the numbered
   tree. -}

numberTree Leaf counter =
  (counter, Leaf)
{- For the 'Leaf' case, we don't update the counter, because we are only
   counting Nodes. In our simulation, we return the counter
   unmodified, along with the Tree. -}

numberTree (Node l x r) counter =
  let (counter0, l') = numberTree l counter
      number         = counter0
      counter1       = counter0+1
      (counter2, r') = numberTree r counter1
  in (counter2, Node l' (x, number) r')
{- The 'Node' case is more complex. In overall structure it is similar
   to the cascade of 'case's in the use of 'Maybe' to simulate
   exceptions from the previous lecture. First we run 'numberTree l'
   to number the left subtree, passing in the value of the
   counter. This returns the new value of the counter, which we call
   'counter0', and the numbered left subtree. Then we take the current
   value of the counter ('counter0') and call it 'number'. Then we add
   one to the value 'counter0' to get 'counter1', simulating the step
   of incrementing the counter. Then we run 'numberTree r' to number
   the right subtree, passing in the current value of the counter
   ('counter1'), yielding another new value of the counter,
   'counter2'. Finally, we return the final value of the counter
   'counter2', and the numbered tree. -}


{- Phew! This is messy, and the opportunity to screw up here is
   immense. We have had to be careful to number variables
   consistently, and to always use the most recent variable name
   holding the "current" value of a variable.

   Can we tidy up the mess? Let's try by first isolating a type of
   "simulations of state mutating operations", just as we used 'Maybe'
   as a type of "simulations of fallible operations". Looking at the
   type of 'numberTree', the following looks like it might work: -}

type State a = Int -> (Int, a)

{- With this definition we are saying "A state mutating operation that
   returns 'a's is a function that takes 'Int's (the initial state) and
   returns pairs of 'Int's (the final state) and 'a's (the result value).

   Just as wrote a function called 'return :: a -> Maybe a' to
   simulate fallible operations that return a value without failing,
   we'll also need a function that simulates mutating operations that
   return a value without mutating anything. We can see a prototype
   for this in the 'Leaf' case in 'numberTree' above:

      numberTree Leaf counter = (counter, Leaf)

   this returns 'Leaf', while simulating *not* mutating the
   counter. Let's capture this pattern as a function: -}

returnState :: a -> State a
returnState x s = (s, x)

{- So 'returnState "hello"' is a (simulation of) a mutating operation
   that does no mutation and returns "hello".

   Following the example of 'Maybe' from the last lecture, the next
   step is to tidy up the steps of 'numberTree' that simulate
   execution of steps one after the other, passing the value of the
   current state through. Let's look at the example again:

       let (counter0, l') = numberTree l counter
           .. continuation code using 'counter0' and l' ..

   We have the same pattern of "return a result and use it in the
   continuation" as we did for 'Maybe' and exceptions. Let's try
   writing down the a similar type: -}

andThen :: State a -> (a -> State b) -> State b
{- The idea is that we do a state mutating operation to get an 'a', then
   we use that 'a' to do another state mutating operation to get a
   'b'. The whole thing is a state mutating operation that returns a
   'b'.

   The implementation looks like: -}
andThen op k = \s ->
  let (s0, a) = op s
      (s1, b) = k a s0
  in (s1, b)

{- The implementation is very terse, but it states:

      to do operation 'op' with continuation 'k', starting from state
      value 's', first run 'op' on 's' to get the state 's0' and value
      'a'. Then run the continuation 'k' with the value 'a' and pass
      in the state value 's0'. This yields the final state 's1' and
      value 'b', which we return.

   We are not yet quite at the stage that we can rewrite 'numberTree'
   using 'returnState' and 'andThen', because we have no functions for
   accessing or updating the state. We do this with functions 'get'
   and 'put': -}

get :: State Int
get = \s -> (s,s)

put :: Int -> State ()
put s' = \s -> (s',())

{- The type of 'get' says that it is a state mutating operation that
   returns an 'Int'. The implementation says that it is the operation
   that does not actually mutate the state, but does return the
   current state.

   The type of 'put' says that it is a state mutating operation that
   takes an 'Int' and returns an 'Int'. It mutates the state by
   replacing it by its 'Int' argument, and returns the value '()',
   representing no useful information (the "unit type", written '()',
   has one constructor '()'. It is often used in these simulations of
   side effects to report that no value is returned, only that some
   side effect has been performed).

   We can now use 'returnState', 'andThen', 'get', and 'put' to
   rewrite 'numberTree' to hide all the "plumbing". The rewriting
   makes it all look very similar to 'lookupAll' from the last
   lecture: -}

numberTree_v2 :: Tree a -> State (Tree (a, Int))
numberTree_v2 Leaf =
  returnState Leaf
numberTree_v2 (Node l x r) =
  numberTree_v2 l `andThen` \l' ->
  get             `andThen` \i  ->
  put (i+1)       `andThen` \() ->
  numberTree_v2 r `andThen` \r' ->
  returnState (Node l' (x, i) r')

{- We can further simplify by defining our own state mutating operations
   that do exactly what we want to the state. For example, if we
   notice that the sequence "get then put incremented by one" happens
   a lot in programs (this is similar to the expression 'x++' in C and
   Java), then we can write it as another state mutating operation: -}

getAndIncrement :: State Int
getAndIncrement s = (s+1,s)

{- And use it to shorten 'numberTree' again: -}

numberTree_v3 :: Tree a -> State (Tree (a, Int))
numberTree_v3 Leaf = return Leaf
numberTree_v3 (Node l x r) =
  numberTree_v3 l `andThen` \l' ->
  getAndIncrement `andThen` \i  ->
  numberTree_v3 r `andThen` \r' ->
  return (Node l' (x, i) r')

{- We could have also written 'getAndIncrement' in terms of 'get',
   'put', 'andThen' and 'returnState': -}

getAndIncrement_v2 :: State Int
getAndIncrement_v2 =
  get       `andThen` \i ->
  put (i+1) `andThen` \() ->
  return i

{- Finally, how do we get from a state mutating operation back to the
   world of just values? How do we run the simulation? We have to
   provide an initial state. Since we are simulating state mutating
   operations by using functions, this is a case of applying the
   simulation of the state mutation to the initial value of the state,
   to get the final value and the result: -}

runState :: State a -> Int -> (Int,a)
runState t i = t i

{-      Part II : Programming with Printing

   Another side effect often used in programs is printing: outputting
   stuff to the screen. Let's see how to simulate this in Haskell, and
   whether we can identify similar patterns as we did in exceptions
   and mutable state.

   We'll simulate printing by requiring that functions that do
   "printing" return a list of 'String's, representing the lines of
   output that they produce.

   For our example this time, we'll write a function that goes through
   a tree of 'Int's adding up all the numbers, while also printing out
   each number as it comes to it. The implementation looks a bit
   similar to the state mutating one above, except with the major
   differences that there is no initial state, and the way the final
   state is computed. -}

printAndSum :: Tree Int -> ([String], Int)
{- The type signature states what we said above: the function takes a
   'Tree Int' and returns a pair of lines of output, and the sum of
   the 'Int's in the tree. -}

printAndSum Leaf = ([], 0)
{- In the 'Leaf' case, we produce no lines of output ('[]') and the sum
   of a 'Leaf' is 0. -}

printAndSum (Node l x r) =
  let (o1, lsum) = printAndSum l
      o2         = [show x]
      (o3, rsum) = printAndSum r
  in (o1 ++ o2 ++ o3, lsum + x + rsum)
{- In the 'Node' case, we printAndSum the left subtree, getting output
   'o1' and sum 'lsum'. Then we use 'show' to turn 'x' into a
   'String', and call the line of output for this node 'o2'. Then we
   'printAndSum' the right subtree, calling the output 'o3' and the
   sum 'rsum'. Finally, we concatenate all the outputs, and add up the
   lsum, r, and rsum.

   Just as with the exception and mutable state examples, this is
   messy, because all the plumbing is exposed, and there is again
   great opportunity for messing up due to all the repetitive code.

   As with exceptions and mutable state, let's tidy this up by first
   identifying the core type used in our simulation of
   printing. Looking at the type of 'printAndSum' above, we see that
   the result is a pair of the list of lines printed, and the actual
   result. Let's turn this into a type definition: -}

type Printing a = ([String],a)

{- This definition says "An operation that does some printing and
   returns a value of type 'a' is simulated using a pair of a list of
   strings and the actual result".

   As before, we need a way to say that we want to return a value
   without doing any printing. We define 'returnPrinting' to do this: -}

returnPrinting :: a -> Printing a
returnPrinting x = ([], x)

{- The type of 'returnPrinting' says that it takes a value of type 'a'
   and returns a (simulation of) an operation that may do some
   printing and returns a value of type 'a'. The implementation says
   that it in fact does no printing (using the empty list of lines),
   and returns the value given as an argument.

   'returnPrinting' suffices to tidy up the 'Leaf' case of the
   'printAndSum' function. To tidy up the plumbing of printing, we
   look at a prototypical case of doing an operation with some
   printing that returns a result 'a', and then running another
   printing operation that requires 'a'. Finally, we concatenate the
   two lists of lines, and the final returned value. This will look
   like:

       let (o1, a) = op
           (o2, b) = k a
       in (o1 ++ o2, b)

   Let's turn this into a definition. As for mutable state and
   exceptions, we take an operation to do first, a continutation, and
   return an operation that does both. -}

andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b
andThenWithPrinting op k =
  let (o1, a) = op
      (o2, b) = k a
  in (o1 ++ o2, b)

{- If we only have the ability to do no printing ('returnPrinting') and
   to sequence a printing operation and a continuation
   ('andThenWithPrinting'), then we won't actually do any printing! To
   fix this, we define a primitive printing operation, 'printLine'
   that takes a string to print and simulates printing it by returning
   a list of lines with only that string in it: -}

printLine :: String -> Printing ()
printLine s = ([s], ())

{- With this type definition and three functions, we can tidy up the
   definition of 'printAndSum' in a way that is very similar to the
   ways we tidied up the exceptions and mutable state examples before: -}

printAndSum_v2 :: Tree Int -> Printing a
printAndSum_v2 Leaf =
  returnPrinting 0
printAndSum_v2 (Node l x r) =
  printAndSum_v2 l   `andThenWithPrinting` \lsum ->
  printLine (show x) `andThenWithPrinting` \() ->
  printAndSum_v2 r   `andThenWithPrinting` \rsum ->
  returnPrinting (lsum + x + rsum)


{-     Part III : What's the continuation?

   We've now seen three examples of simulations of side effects:
   exceptions, mutable state, and printing. In the case of exceptions,
   we simulated them by using the 'Maybe' type. For mutable state, the
   simulation worked by passing the state through the function as an
   extra parameter. For printing, the simulation collected all the
   printed strings in a list.

   In each case, writing the code out explicitly was messy -- it is
   boring to write, and error prone due to the large amount of
   repetition. We were able to tidy up the examples by first writing a
   type synonym that captured the essence of the way we were
   simulating the side effect, and then by writing functions that
   helped us hide the plumbing. In each of the three cases, we had a
   function that simulated "doing nothing" while returning a value:

       return         :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a

   And in each of the three cases, we had a function that simulated
   "sequencing" of a side effecting operation before a continuation:

       ifOK                :: Maybe a    -> (a -> Maybe b)    -> Maybe b
       andThen             :: State a    -> (a -> State b)    -> State b
       andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b

   And then for each of the three cases, we had special functions
   peculiar to that kind of side effect. For exceptions, we had
   'failure' and 'catch'. For mutable state, we had 'get' and 'put',
   and for printing we had 'printLine'.

   But for all the cases we've seen so far, there was a common core:
   the "do nothing" operation, and the "do this and then do that"
   operation. In the next lecture, we'll define a type class that
   gives types that support this pair of operations a name "Monad",
   and investigate some more examples. -}
