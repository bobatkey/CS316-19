{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex1ResitTest where

import Prelude hiding (words)
import Test.QuickCheck (quickCheck, (==>), Property)
import Ex1

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 1 : FIRST-ORDER PROGRAMMING               -}
{-                                                                    -}
{-    * * *  RESIT TEST QUESTIONS  * * *                              -}
{----------------------------------------------------------------------}

{- Your combined score from the submission and the test will be worth
   30% of the overall marks for the class (so one mark is worth half a
   percent).

   This file contains the test questions. Answer the questions in this
   file, and make sure that both are committed to GitLab before
   arranging a call with the course lecturer (Robert Atkey
   <robert.atkey@strath.ac.uk>) -}

{----------------------------------------------------------------------}
{- PART 1 : FUN(ctional) WITH LISTS AND TREES                         -}
{----------------------------------------------------------------------}

{- 1.1.7 Separating into chunks.

   For this question, you will write a function that performs the
   opposite of 'concatLists'. Instead of joining together a list of
   lists, 'chunk' separates a list into a list of lists ("chunks") of
   a given size. Some examples:

      chunk 2 [] "hello" == ["he", "ll", "o"]
      chunk 3 [] "hello" == ["hel", "lo"]
      chunk 1 [] "hello" == ["h", "e", "l", "l", "o"]

   The function 'chunk' takes three arguments. The first argument is
   the chunk size. You can assume that this is always greater than
   0. The second argument is an 'accumulator' that gathers the current
   chunk (in reverse) until it reaches the chunk size. When calling
   'chunk' normally, this argument is the empty list '[]', because the
   starting chunk is empty. The third argument is the input list.

   There are four cases of interest:

   1. We have run out of input, and the current chunk is empty. Return
   the empty list.

   2. We have run out of input, but the current chunk contains
   something. Return a singleton list containing this chunk.

   3. We have an element of input 'x', but the current chunk is
   full. Return the current chunk, and recursively start a new chunk
   with 'x'.

   4. We have an element of input 'x', and the current chunk is not
   full. Add 'x' to the current chunk, and continue.

   To measure the size of the current chunk, use the function
   'length'.

   Write 'chunk' here: -}

chunk :: Int -> [a] -> [a] -> [[a]]
chunk = undefined

{- 3 MARKS -}

{- 1.1.8 Property of 'concatLists' and 'chunk'.

   In Question 1.1.4, I said that 'chunk' performs the opposite of
   'concatLists'. Write this as a QuickCheck property. I have given
   you the precondition that 'chunkSize' is greater than 0. You fill
   in the remainder of the property where is says "False". -}

chunkProp :: Int -> [Int] -> Property
chunkProp chunkSize xs =
  chunkSize > 0 ==> False -- replace "False" here with something useful

{- 2 MARKS -}

{- 1.1.9 Splitting lines at commas.

   Write a function that takes a list of 'String's, and splits each
   one (using 'splitWith') at the commas. Examples:

     splitAllAtCommas ["a,b","c,d","e"] == [["a","b"],["c","d"],["e"]]
     splitAllAtCommas []                == []

   Do not use 'map' to write your function. Write it using recursion. -}

splitAllAtCommas :: [String] -> [[String]]
splitAllAtCommas = undefined

{- 2 MARKS -}

{- 1.1.10 Splitting into records.

   Using 'splitOn' and 'splitAllAtCommas', write a function that
   splits a 'String' into lines (by splittig at each occurence of
   '\n') and then each line into comma separated values. Examples

      splitCSV "a,b\nc,d\ne" == [["a","b"],["c","d"],["e"]]
      splitCSV ""            == []
-}

splitCSV :: String -> [[String]]
splitCSV = undefined

{- 1 MARK -}

{----------------------------------------------------------------------}
{- PART II : CURSORS                                                  -}
{----------------------------------------------------------------------}

{- 1.2.6 Replacement.

   Write a function that replaces the element under the cursor with
   the one supplied as the first argument. If the cursor is at the end
   of the line, then the new character replaces the old end of line
   marker, and the cursor is placed on the new character. Examples:

     replace 'x' (AtEnd "cba")          == Within "cba" 'x' ""
     replace 'x' (Within "cba" 'd' "")  == Within "cba" 'x' ""
     replace 'x' (Within "cba" 'd' "e") == Within "cba" 'x' "e"
-}

replace :: Char -> Cursor -> Cursor
replace = undefined

{- 2 MARKS -}


{- 1.2.7 Multiple lines.

   The Cursor type in the Exercise implements a single line editor. We
   will now upgrade this to a multiline editor. The structure for
   implementing a multiline editor is very similar to the structure
   used for implementing the cursor.

   Here is the 'LineCursor' type: -}

data LineCursor
  = LineCursor [String] Cursor [String]
  deriving Show


{- A 'LineCursor' value has three parts:

       LineCursor above line below

   It consists of a list 'above' holding the lines above the cursor
   (in reverse), the cursor on the current line ('line'), and a list
   containing the lines below the current cursor. Notice the
   similarity to the 'Within' constructor in the 'Cursor'
   datatype. There is no analogue of the 'AtEnd' constructor because
   it is not possible to be off the end of the file.

   Implement the following functions.

   'current' returns the 'Cursor' on the current line.

   'update' replaces the 'line' cursor with a new one.

   'moveUp' simulates going up a line:
     - if the current line is the first one ('before' is empty), then
       the same LineCursor is returned.
     - otherwise, the current line is converted to a String (using
       'fromCursor' from the Ex1.hs file) and put into 'below', and
       the line above is converted to a cursor (using 'toCursor') and
       used as the new current line.

   You can also implement moveDown, if you like. -}

current :: LineCursor -> Cursor
current = undefined

update :: LineCursor -> Cursor -> LineCursor
update = undefined

moveUp :: LineCursor -> LineCursor
moveUp = undefined

{- 6 MARKS -}


{----------------------------------------------------------------------}
{- PART 3 : REPRESENTING PROCESSES                                    -}
{----------------------------------------------------------------------}

{- 1.3.5 Equality Test.

   Write a process that inputs two bits, and then outputs True if they
   are equal, and False if not.

      process equal [True,True]   == [True]
      process equal [False,False] == [True]
      process equal [True,False]  == [False]
      process equal [False,True]  == [False]
-}

equal :: Process
equal = undefined

{- 1 MARK -}


{- 1.3.6 All Truth, all the time.

   Write a version of the 'process' function that acts as if the input
   is an inexhaustible supply of 'True'. That is, when interpreting an
   'Input' constructor, it always picks the true path. The returned
   value should be the bits output by the process, in order. -}

alwaysTrue :: Process -> [Bool]
alwaysTrue = undefined

{- 2 MARKS -}


{- 1.3.7 Duplicating all outputs.

   Write a function that rewrites processes so that every time the
   original process outputs once, the rewritten process outputs
   twice. Otherwise, the rewritten process acts like the
   original. Examples:

       process (dupOutputs copyCat) [True]  == [True, True]
       process (dupOutputs copyCat) [False] == [False, False]
-}

dupOutputs :: Process -> Process
dupOutputs = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE  (TEST RESIT QUESTIONS)                            -}
{----------------------------------------------------------------------}
