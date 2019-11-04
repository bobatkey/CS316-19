module Lec11Live where

{-      LECTURE 11 : PROGRAMMING WITH SIDE EFFECTS -}

{- Previously:

   - Simulating exceptions using 'Maybe'

   - Thinking of a value of type 'Maybe a' as an operation that may
     fail by throwing an exception.

   - Two useful operations:

        return :: a -> Maybe a
           -- an operation that always succeeds

        ifOK   :: Maybe a -> (a -> Maybe b) -> Maybe b
           -- try to run a fallible operation, if it succeeds then
              run the continuation.

   Does this work with other kinds of side effects? -}





{-    Part I : Programming with Mutable State


   Some "Java" code:

      int x = 5;
       .. some code where x is 5 ..
      x = x + 5;
       .. some code where x is 10 ..
      while (<blah>) {
         x += 1;
         .. x gets 1 bigger every time the loop executes ..
      }
      .. x could be some number bigger than 10 ..

   What if we couldn't alter ("mutate") variables? Could rename them:

      int x0 = 5;
       .. some code where x0 is 5 ..
      int x1 = x0 + 5;
       .. some code where x1 is 10 ..
      loop x =
        int x2 = x + 1
        ...
        if not stop then loop x2 else x2
      int x3 = loop x1
      .. x3 could be some number bigger than 10 ..


   We'll use this idea to simulate state in Haskell. (Also used in
   modern compilers for compiling code that uses mutable state: Static
   Single Assignment form (SSA)) -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Numbering the nodes:

      Node (Node Leaf "A"     Leaf) "B"     (Node Leaf "C"     Leaf)

   to

      Node (Node Leaf ("A",0) Leaf) ("B",1) (Node Leaf ("C",2) Leaf)
-}


numberTree :: Tree a -> (Int -> (Int, Tree (a, Int)))
numberTree Leaf counter = (counter, Leaf)
numberTree (Node l x r) counter0 =
  let (counter1, l') = numberTree l counter0
      number         = counter1
      counter2       = counter1 + 1
      (counter3, r') = numberTree r counter2
  in (counter3, Node l' (x, number) r')

-- State type
type State a = Int -> (Int, a)

-- returnState
returnState :: a -> State a
returnState x = \s -> (s, x)

-- andThen
andThen :: State a -> (a -> State b) -> State b
andThen op k = \s0 ->
   let (s1, a) = op s0
       (s2, b) = k a s1
   in (s2, b)

-- get
get :: State Int
get = \s -> (s,s)

-- data () = ()

-- put
put :: Int -> State ()
put i = \s -> (i,())

numberTree_v2 :: Tree a -> State (Tree (a, Int))
numberTree_v2 Leaf =
  returnState Leaf
numberTree_v2 (Node l x r) =
  numberTree_v2 l   `andThen` \l' ->
  get               `andThen` \number ->
  put (number+1)    `andThen` \() ->
  numberTree_v2 r   `andThen` \r' ->
  returnState (Node l' (x, number) r')
{-
  let (counter1, l') = numberTree l counter0
      number         = counter1
      counter2       = counter1 + 1
      (counter3, r') = numberTree r counter2
  in (counter3, )
-}

runState :: State a           -> Int -> (Int, a)
        --  (Int -> (Int, a)) -> Int -> (Int, a)
runState op = op


{-      Part II : Programming with Printing -}


printAndSum :: Tree Int -> ([String], Int)
printAndSum Leaf =
  ([], 0)
printAndSum (Node l x r) =
  let (o1, lsum) = printAndSum l
      o2         = [show x]
      (o3, rsum) = printAndSum r
  in (o1 ++ o2 ++ o3, lsum + x + rsum)

-- Printing type
type Printing a = ([String], a)

-- returnPrinting
returnPrinting :: a -> Printing a
returnPrinting x = ([], x)

-- andThenWithPrinting
andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b
andThenWithPrinting op k =
  let (o1, a) = op
      (o2, b) = k a
  in (o1 ++ o2, b)

-- printLine
printLine :: String -> Printing ()
printLine s = ([s], ())

-- printAndSum_v2
printAndSum_v2 :: Tree Int -> Printing Int
printAndSum_v2 Leaf =
  returnPrinting 0
printAndSum_v2 (Node l x r) =
  printAndSum_v2 l   `andThenWithPrinting` \lsum ->
  printLine (show x) `andThenWithPrinting` \() ->
  printAndSum_v2 r   `andThenWithPrinting` \rsum ->
  returnPrinting (lsum + x + rsum)


{-     Part III : What's the continuation? -}

{-
       return         :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a
-}

{-
       ifOK                :: Maybe a    -> (a -> Maybe b)    -> Maybe b
       andThen             :: State a    -> (a -> State b)    -> State b
       andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b

       sequ                :: Process a  -> (a -> Process b)  -> Process b
-}

--    Monad
--    Warm Fuzzy Thing

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
