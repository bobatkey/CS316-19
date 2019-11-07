module RecSchemesQuestions where

import Prelude hiding (sum, foldr)

{----------------------------------------------------------------------}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = x `f` (foldr f a xs)

{-
          foldr f a [x1,x2,x3]
      ==  f x1 (foldr f a [x2,x3])
      ==  f x1 (f x2 (foldr f a [x3]))
      ==  f x1 (f x2 (f x3 (foldr f a [])))
      ==  f x1 (f x2 (f x3 a))

          x1  :  (x2  :  (x3   : []))

          x1 `f` (x2 `f` (x3 `f` a))
-}

-- How does foldr (/) 2 [8,12,24,4] work?

--    8 : (12 : (24 : (4 : [])))
--    8 / (12 / (24 / (4 / 2)))
--                    2
--               12
--         1
--    8



{----------------------------------------------------------------------}
{- EXERCISE: the following recursive function returns the list it is
   given as input: -}

listIdentity :: [a] -> [a]
listIdentity []     = []
listIdentity (x:xs) = x : listIdentity xs

{- Write this function as a 'foldr': -}

--    8 : (12 : (24 : (4 : [])))
--    8 : (12 : (24 : (4 : [])))


{----------------------------------------------------------------------}
{- EXERCISE: the following recursive function does a map and a filter at
   the same time. If the function argument sends an element to
   'Nothing' it is discarded, and if it sends it to 'Just b' then 'b'
   is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr': -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f xs = foldr (\x filtered ->
                           case f x of
                             Nothing -> filtered
                             Just b  -> b : filtered)-- what happens to a colon?
                        [] -- what happens to the empty input?
                        xs


{----------------------------------------------------------------------}
{- Numbers from digits -}

-- Given a list of digits, generate the corresponding number

--   [1,2,3,4,5]
--   12345

digitsToNumber :: [Int] -> Int
digitsToNumber xs = foldr (\d n -> n*10+d) 0 (reverse xs)

  -- foldl (\n d -> n*10+d) 0 xs

  -- int total = 0;
  -- for (int d : digits) {
  --    total = total*10 + d;
  -- }

--    foldl (\n d -> n+d*10) 1 [1,2,3]
-- =  foldl (\n d -> n+d*10) 11 [2,3]
-- =  foldl (\n d -> n+d*10) 31 [3]
-- =  foldl (\n d -> n+d*10) 61 []
-- =  61

-- ((1*10)+2)*10+3

   -- "Engineering solution"
   -- read (foldr (\x rest -> show x ++ rest) [] xs)

{----------------------------------------------------------------------}
{- Folding over trees -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf = l
foldTree l n (Node left x right) = n (foldTree l n left) x (foldTree l n right)

-- 1. Doing nothing

listIdentity' :: [a] -> [a]
listIdentity' = foldr (:) []

treeIdentity :: Tree a -> Tree a
treeIdentity = foldTree Leaf (\l x r -> Node l x r)

-- 2. Size of a tree
sizeTree :: Tree a -> Int
sizeTree = foldTree 0 -- how big is a Leaf?
                    (\l x r -> 1 + l + r) -- how to work out size of a Node?


-- 3. Mirroring
mirror :: Tree a -> Tree a
mirror Leaf         = Leaf
mirror (Node l x r) = Node (mirror r) x (mirror l)

mirror' :: Tree a -> Tree a
mirror' = foldTree Leaf (\l x r -> Node r x l)

-- 4. Finding things
find :: Eq a => a -> Tree a -> Bool
find a Leaf = False
find a (Node l x r) = a == x || find a l || find a r

find' :: Eq a => a -> Tree a -> Bool
find' a = foldTree False (\l x r -> a == x || l || r)

{----------------------------------------------------------------------}
{- EXERCISE: The following is a datatype of Natural Numbers (whole
   numbers greater than or equal to zero), represented in unary. A
   natural number 'n' is represented as 'n' applications of 'Succ' to
   'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe as
   above, work out the type and implementation of a 'fold' function
   for 'Nat's. -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

-- 4
-- Succ (Succ (Succ (Succ Zero)))

foldNat :: b -> (b -> b) -> Nat -> b
foldNat zero succ Zero     = zero
foldNat zero succ (Succ n) = succ (foldNat zero succ n)

-- foldNat :: b -> (Nat -> b -> b) -> Nat -> b
-- foldNat zero succ Zero     = zero
-- foldNat zero succ (Succ n) = succ n (foldNat zero succ n)

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}

natIdentity :: Nat -> Nat
natIdentity = foldNat Zero Succ

add :: Nat -> Nat -> Nat
add x y = foldNat y (\x_add_y -> Succ x_add_y) x

  -- (Succ x) + y
  --
  --       x  + y
