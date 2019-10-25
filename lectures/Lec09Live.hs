module Lec09Live where

import Prelude hiding ( Monoid (..)
                      , Foldable (..)
                      , Functor(..)
                      , (<$>)
                      , all
                      , map
                      , length
                      , Maybe (..) )

{-        LECTURE 09 : FUNCTORS AND CONTAINERS -}

-- Some data stuctures that 'contain' other data:

-- data [a] = [] | a : [a]

data Tree a = Leaf | Node (Tree a) a (Tree a)

data Maybe a = Nothing | Just a


{-    PART I : FUNCTORS -}

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

{-
          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]
-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node l x r) =
  Node (mapTree f l) (f x) (mapTree f r)

{-
      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)
-}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just a) = Just (f a)

{-
       Nothing      or       Just a
         |                        |
         v                        v
       Nothing               Just b
-}

-- compare the types
--   map      :: (a -> b) -> [a]     -> [b]
--   mapTree  :: (a -> b) -> Tree a  -> Tree b
--   mapMaybe :: (a -> b) -> Maybe a -> Maybe b

-- the functor type class
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Tree where
  -- mapTree :: (a -> b) -> Tree a -> Tree b
  fmap = mapTree

instance Functor Maybe where
  fmap = mapMaybe

data IntFun a = IntFun (Integer -> a)

instance Functor IntFun where
  fmap f (IntFun g) = IntFun (\i -> f (g i))

--

-- From Ex2: Picture and Process

-- Non-instance

data Fun a = MkFun (a -> a)
  -- uses 'a' in an input position

instance Functor Fun where
  fmap f (MkFun g) = MkFun (\b -> b)

-- why not? laws

--    fmap (\y -> y) x   = x     (identity law)
--    fmap g (fmap f x)  =  fmap (\y -> g (f y)) x
--                                  (fusion / composition)

-- using it

-- 1. as shorthand

data Process
  = End
  | Output Bool Process
  | Input Process Process
  deriving Show

process2 :: Process -> [Bool] -> Maybe ([Bool], [Bool])
process2 End           bs     = Just ([], bs)
process2 (Input tp fp) []     = Nothing
process2 (Input tp fp) (b:bs) =
  process2 (if b then tp else fp) bs
process2 (Output b p)  bs     =
{-  case process2 p bs of
    Nothing       -> Nothing
    Just (os, bs) -> Just (b : os, bs)-}
  (\(os,bs) -> (b:os,bs)) <$> process2 p bs

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- 2. being generic
-- 3. as a building block

{-        PART II : FOLDABLE -}

-- seen 'folds'

--   concatLists
--   total
--   and
concatLists :: [[a]] -> [a]
concatLists [] = []
concatLists (xs:xss) = xs ++ concatLists xss

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

-- observation: many data structures are "list like"

-- foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
-- foldTree

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- but the recursion schemes don't easily let us access that

-- make a new abstraction: Monoids
--  Ints
--  lists
--  booleans

class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

--   mempty `mappend x  = x
--   x `mappend` mempty = x
--   (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid Int where
  mempty = 0
  mappend = (+)

instance Monoid [a] where
  mempty = []
  mappend = (++)

instance Monoid Bool where
  mempty = True
  mappend = (&&)

foldList :: Monoid m => [m] -> m
foldList []     = mempty
foldList (m:ms) = m `mappend` foldList ms

-- folding a list
--    recover the behaviour from the type
-- folding a tree
foldTree :: Monoid m => Tree m -> m
foldTree Leaf = mempty
foldTree (Node l m r) =
  foldTree l `mappend` m `mappend` foldTree r

-- folding a maybe
foldMaybe :: Monoid m => Maybe m -> m
foldMaybe Nothing  = mempty
foldMaybe (Just m) = m

-- folding a function?

-- foldList  :: Monoid m => [m]     -> m
-- foldTree  :: Monoid m => Tree m  -> m
-- foldMaybe :: Monoid m => Maybe m -> m

class Foldable f where
  fold :: Monoid m => f m -> m

instance Foldable [] where fold = foldList
instance Foldable Tree where fold = foldTree
instance Foldable Maybe where fold = foldMaybe

-- foldMap
foldMap :: (Functor f, Foldable f, Monoid m) =>
           (a -> m) ->
           f a ->
           m
foldMap f c = fold (fmap f c)

glength :: (Functor f, Foldable f) => f a -> Int
glength = foldMap (\_ -> 1)

gtotal :: (Functor f, Foldable f) => f Int -> Int
gtotal = foldMap (\x -> x)

gall :: (Functor f, Foldable f) => (a -> Bool) -> f a -> Bool
gall f = foldMap f

-- exists,all,length
