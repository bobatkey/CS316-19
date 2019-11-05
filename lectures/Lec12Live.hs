{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
module Lec12Live where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Data.Char      (isDigit, digitToInt)


{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y

{-       Lecture 12 : MONADS -}



{-
       return         :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a
-}

{-
       ifOK                :: Maybe a ->    (a -> Maybe b)    -> Maybe b
       andThen             :: State a ->    (a -> State b)    -> State b
       andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b
       sequ                :: Process a ->  (a -> Process b)  -> Process b
-}


-- Monad type class
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

{-    Part I : 'Maybe' is a Monad -}

instance Monad Maybe where
  return :: a -> Maybe a
  return x = Just x

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  op >>= k = case op of
               Nothing -> Nothing
               Just x  -> k x

failure :: Maybe a
failure = Nothing

catch :: Maybe a -> Maybe a -> Maybe a
catch op1 op2 =
  case op1 of
    Nothing -> op2
    Just x  -> Just x


{-    Part II : 'do' notation -}

{-
  op1   >>= \x ->
  op2 x >>= \y ->
  ..

  do x <- op1
     y <- op2 x
     ...
-}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []            = failure
search k ((k',v'):kvs) = if k == k' then return v' else search k kvs

lookupAll :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll kvs Leaf = return Leaf
lookupAll kvs (Node l k r) =
  do l' <- lookupAll kvs l
     v  <- search k kvs
     r' <- lookupAll kvs r
     return (Node l' v r')

  -- lookupAll kvs l >>= \l' ->
  -- search k kvs    >>= \v ->
  -- lookupAll kvs r >>= \r' ->
  -- return (Node l' v r')

{-   do x <- e1      becomes    e1 >>= \x -> do e2
        e2

     do e1           becomes    e1 >> e2
        e2

       (>>) op1 op2 = op1 >>= \_ -> op2

     do e            becomes    e

     do l' <- lookup kvs l   becomes   lookup kvs l >>= \l' ->
        r' <- lookup kvs r             lookup kvs r >>= \r' ->
        return (l', r')                return (l', r')
-}


{-    Part III : 'State' is a monad -}

-- type State a = Int -> (Int, a)

newtype State a = MkState (Int -> (Int, a))

runState :: State a -> (Int -> (Int, a))
runState (MkState t) = t

instance Monad State where
  return :: a -> State a
  return x = MkState (\s -> (s,x))

  (>>=)  :: State a -> (a -> State b) -> State b
  op >>= k = MkState (\s ->
                        let (s1,a) = runState op s
                            (s2,b) = runState (k a) s1
                        in (s2, b))

get :: State Int
get = MkState (\s -> (s,s))

put :: Int -> State ()
put i = MkState (\s -> (i,()))

getAndIncrement :: State Int
getAndIncrement =
  do x <- get
     put (x+1)
     return x

{-    Part IV : 'Printing' is a monad -}

-- type Printing a = ([String], a)

data Printing a = MkPrinting [String] a
  deriving Show

instance Monad Printing where
  return :: a -> Printing a
  return x = MkPrinting [] x

  (>>=) :: Printing a -> (a -> Printing b) -> Printing b
  op >>= k =
    let MkPrinting out1 a = op
        MkPrinting out2 b = k a
    in MkPrinting (out1 ++ out2) b

printLine :: String -> Printing ()
printLine s = MkPrinting [s] ()

add :: Int -> Int -> Printing Int
add x y =
  do printLine ("Adding " ++ show x ++ " and " ++ show y)
     return (x+y)


{-      Part V : Writing code that works for all 'Monad's -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) =
  do y  <- f x
     ys <- mapM f xs
     return (y:ys)

-- ['1','2','4','5', 'A']
-- Nothing


readDigits :: [Char] -> Maybe [Int]
readDigits = mapM (\c -> if isDigit c then
                           return (digitToInt c)
                         else
                           failure)

mapM_ :: Monad m => (a -> m ()) -> [a] -> m ()
mapM_ f [] = return ()
mapM_ f (x:xs) =
  do f x
     mapM_ f xs

printList :: [String] -> Printing ()
printList xs = mapM_ printLine xs

for_ :: Monad m => [a] -> (a -> m ()) -> m ()
for_ xs f = mapM_ f xs

printList_v2 :: [String] -> Printing ()
printList_v2 xs =
  for_ xs (\x -> printLine x)

printNumbers :: Printing ()
printNumbers =
  for_ [1..10] (\x -> printLine (show x))

lengthImp :: [a] -> State Int
lengthImp xs =
  do put 0
     for_ xs (\_ -> do len <- get
                       put (len+1))
     len <- get
     return len

len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

sumImp :: [Int] -> State Int
sumImp xs =
  do put 0
     for_ xs (\x -> do sum <- get
                       put (sum+x))
     sum <- get
     return sum


-- IO

getChar :: IO Char
putChar :: Char -> IO ()
