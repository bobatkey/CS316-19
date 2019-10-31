module HOFSolutions where

import Prelude hiding (flip)
import Data.Char (toUpper)

{-    HIGHER ORDER FUNCTIONS

   These questions are about the material in Lecture 05 "Higher order
   functions". -}


{- Q.0 Lambda notation.

   Rewrite the following functions using the '\x -> e' notation (the
   "lambda" notation), so that they are written as 'double =
   <something>', and so on. -}

double :: Int -> Int
double x = 2*x

mul :: Int -> Int -> Int
mul x y = x * y

invert :: Bool -> Bool
invert True  = False
invert False = True
  {- HINT: use a 'case', or an 'if'. -}

{- ANSWER:

     double = \x -> 2*x

     mul = \x -> \y -> x * y

       OR

     mul = \x y -> x * y

     invert = \x -> case x of
                      True -> False
                      False -> True

       OR

     invert = \x -> if x then False else True
-}


{- Q.1 Partial Application

   The function 'mul' defined above has the type 'Int -> Int ->
   Int'. (a) What is the type of the Haskell expression:

       mul 10

   (b) what is 'mul 10'? How can you use it to multiply a number? -}



{- ANSWER:
   (a) The type f 'mul 10' is 'Int -> Int'

   (b) 'mul 10' is a function expecting to be given an 'Int'. When it
       is given an 'Int', it will multiply it by 10 and return the
       result. It can be used to multiply a number by applying it to
       another number:

           mul 10 2

       yields 20. -}




{- Q.2 Partial Application

   Write the 'double' function above using 'mul'. Can you make your
   function as short as possible? -}

double_v2 :: Int -> Int
double_v2 = undefined -- fill this in


{- ANSWER:

     double_v2 = mul 2

-}


{- Q.3 Using 'map'.

   The function 'toUpper' takes a 'Char' and turns lower case
   characters into upper cases one. All other characters it returns
   unmodified. For example:

       > toUpper 'a'
       'A'
       > toUpper 'A'
       'A'

   Strings are lists of characters. 'map' is a function that applies a
   function to every character in a list and returns a new list.

   Write the function 'shout' that uppercases a string, so that:

      > shout "hello"
      "HELLO"
-}

shout :: String -> String    -- remember that String = [Char]
shout = undefined

{- ANSWER:

     shout = map toUpper

   OR

     shout str = map toUpper str
-}

{- Q.4 Using 'map' with another function.

   The function 'concat' does what the function 'concatLists' from
   Exercise 1 did:

      > concat [[1,2],[3,4],[5,6]]
      [1,2,3,4,5,6]

   Using 'map', 'concat', and either a helper function or a function
   written using '\', write a function 'dupAll' that duplicates every
   element in a list. For example:

      > dupAll [1,2,3]
      [1,1,2,2,3,3]
      > dupAll "my precious"
      "mmyy  pprreecciioouuss"

   HINT: try writing a helper function that turns single elements into
   two element lists. -}

dupAll :: [a] -> [a]
dupAll = undefined

{- ANSWER:

   With a helper function:

     dup1 :: a -> [a]
     dup1 x = [x,x]

     dupAll xs = concat (map dup1 xs)

   With a lambda ('\') function:

     dupAll xs = concat (map (\x -> [x,x]) xs)

   Shorter version, using a pipeline of composed functions:

     dupAll = concat . map (\x -> [x,x])
-}

{- Q.5 Using 'filter'

   (a) Use 'filter' to return a list of consisting of only the 'E's in
       a 'String'.

   (b) Use 'onlyEs' and 'length' to count the number of 'E's in a string.

   (c) Write a single function that takes a character 'c' and a string
       's' and counts the number of 'c's in 's'. -}

onlyEs :: String -> String
onlyEs = undefined

numberOfEs :: String -> Int
numberOfEs = undefined

numberOf :: Char -> String -> Int
numberOf = undefined

{- ANSWER:

     onlyEs :: String -> String
     onlyEs s = filter (\x -> x == 'E') s

     numberOfEs :: String -> Int
     numberOfEs s = length (onlyEs s)

     numberOf :: Char -> String -> Int
     numberOf c = length . filter (\x -> x == c)

         OR

     numberOf c s = length (filter (\x -> x == c) s)
-}

{- Q.6 Rewriting 'filter'

   (a) Write a function that does the same thing as filter, using
      'map' and 'concat'.

   (b) Write a function that does a 'map' and a 'filter' at the same
       time, again using 'map' and 'concat'.
-}

filter_v2 :: (a -> Bool) -> [a] -> [a]
filter_v2 = undefined

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = undefined

{- ANSWER:

   filter_v2 :: (a -> Bool) -> [a] -> [a]
   filter_v2 p = concat . map (\x -> if p x then [x] else [])

   filterMap :: (a -> Maybe b) -> [a] -> [b]
   filterMap f = concat . map (\x -> case f x of
                                       Nothing -> []
                                       Just y  -> [y])
-}

{- Q.7 Evaluating Formulas

   Here is a datatype describing formulas in propositional logic, as
   in CS208 last year. Atomic formulas are represented as 'String's. -}

data Formula
  = Atom String
  | And  Formula Formula
  | Or   Formula Formula
  | Not  Formula
  deriving Show

{- (a) Write a function that evaluates a 'Formula' to a 'Bool'ean value,
       assuming that all the atomic formulas are given the value
       'True'. Note that the following Haskell functions do the basic
       operations on 'Bool'eans:

           (&&) :: Bool -> Bool -> Bool    -- 'AND'
           (||) :: Bool -> Bool -> Bool    -- 'OR'
           not  :: Bool -> Bool            -- 'NOT'
-}

eval_v1 :: Formula -> Bool
eval_v1 = undefined

{- ANSWER:

      eval_v1 (Atom a)    = True
      eval_v1 (And f1 f2) = eval_v1 f1 && eval_v1 f2
      eval_v1 (Or f1 f2)  = eval_v1 f1 || eval_v1 f2
      eval_v1 (Not f)     = not (eval_v1 f)
-}

{- (b) Now write a new version of 'eval_v1' that, instead of evaluating
       every 'Atom a' to 'True', takes a function that gives a 'Bool'
       for each atomic proposition: -}

eval :: Formula -> (String -> Bool) -> Bool
eval = undefined

{- ANSWER:

      eval (Atom a)    v = v a
      eval (And f1 f2) v = eval f1 v && eval f2 v
      eval (Or f1 f2)  v = eval f1 v || eval f2 v
      eval (Not f)     v = not (eval f v)
-}

{- Q.8 Substituting Formulas

   Write a function that, given a function 's' that turns 'String's
   into 'Formula's (a "substitution"), replaces all the atomic
   formulas in a Formula with whatever 'f' tells it to: -}

subst :: (String -> Formula) -> Formula -> Formula
subst = undefined

{- ANSWER:

     subst s (Atom a)    = s a
     subst s (And f1 f2) = And (subst s f1) (subst s f2)
     subst s (Or f1 f2)  = Or (subst s f1) (subst s f2)
     subst s (Not f)     = Not (subst s f)
-}


{- Q.9 Backwards application

   Write a function of the following type that takes a value 'x' and a
   function 'f' and applies 'f' to 'x'. Note that this functions takes
   its arguments in reverse order to normal function application! -}

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

{- ANSWER:

     (|>) x f = f x
-}

{- This function can be used between its arguments like so:

       "HELLO" |> map toLower

   and it is useful for chaining calls left-to-right instead of
   right-to-left as is usual in Haskell:

       "EIEIO" |> filter onlyEs |> length
-}

{- Q.10 Flipping

   Write a function that takes a two argument function as an input,
   and returns a function that does the same thing, but takes its
   arguments in reverse order: -}

flip :: (a -> b -> c) -> b -> a -> c
flip  = undefined

{- ANSWER:

     flip f a b = f b a
-}

{- Q.11 Composition

   Write a function '>>>' that composes two functions: takes two
   functions 'f' and 'g', and returns a function that first runs 'f'
   on its argument, and then runs 'g' on the result.

   HINT: this is similar to the function 'compose' in Lecture 05.
-}

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = undefined

{- Try rewriting the 'numberOfEs' function from above using this one. -}

{- ANSWER:

      (>>>) f g x = g (f x)


      numberOfEs = onlyEs >>> length
-}
