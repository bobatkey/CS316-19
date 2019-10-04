{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec02Live where

import Prelude hiding (Maybe (..))

{-    LECTURE 02 : DEFINING FUNCTIONS -}

-- square, generalising

square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x + x



{-  int square(int x) { return x * x; }   -}

-- four ways of writing 'not'

-- using pattern matching
not :: Bool -> Bool
not True  = False
not False = True

-- using if-then-else
not2 :: Bool -> Bool
not2 x = if x then False else True

-- using guard
not3 :: Bool -> Bool
not3 x
  | x == True = False
  | otherwise = True

isBig :: Int -> Bool
isBig x
  | x >= 5    = True
  | x <= -5   = True
  | otherwise = False

-- Using case
not4 :: Bool -> Bool
not4 x = case x of
           False -> True
           True  -> False

data Direction = Up | Down | Left | Right
  deriving Show

flipVert :: Direction -> Direction
flipVert d = case d of
               Up   -> Down
               Down -> Up
               x    -> x

--   x ? e1 : e2

-- recall 'total'

total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

-- append
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

--    append [1,2] [3,4]
-- =  append (1:2:[]) [3,4]
-- =  1 : append (2:[]) [3,4]
-- =  1 : 2 : append [] [3,4]
-- =  1 : 2 : [3,4]
-- =  [1,2,3,4]

-- rev
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

--   rev [1,2]
-- = rev (1:2:[])
-- = append (rev (2:[])) [1]
-- = append (append (rev []) [2]) [1]
-- = append (append [] [2]) [1]
-- = append [2] [1]
-- = 2 : append [] [1]
-- = 2 : [1]
-- = [2,1]

-- head, and Maybe

data Maybe a
  = Nothing
  | Just a
  deriving (Show, Eq)

hd :: [a] -> Maybe a
hd []     = Nothing
hd (x:xs) = Just x


-- sawPrefix

--    String = [Char]

--   sawPrefix "foo" "foop" == Just "p"
--   sawPrefix "foo" "burp" == Nothing
--   sawPrefix ""    "burp" == Just "burp"

sawPrefix :: [Char] -> [Char] -> Maybe [Char]
sawPrefix []     ys     = Just ys
sawPrefix (x:xs) []     = Nothing
sawPrefix (x:xs) (y:ys)
  | x == y              = sawPrefix xs ys
  | otherwise           = Nothing

--   sawPrefix "foo" "fun"
-- = sawPrefix "oo" "un"
-- = Nothing

--   basename ".md" "my-plans.md"  == Just "my-plans"
--   basename ".md" "my-plans.jpg" == Nothing

basename :: String -> String -> Maybe String
basename suffix str =
  case sawPrefix (rev suffix) (rev str) of
    Nothing  -> Nothing
    Just rts -> Just (rev rts)

-- ".md" "my-plans.md"
-- "dm." "dm.snals-ym"

-- basename
