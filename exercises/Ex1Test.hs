{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex1Test where

import Prelude hiding (words)
import Test.QuickCheck (quickCheck, (==>), Property)
import Ex1

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 1 : FIRST-ORDER PROGRAMMING               -}
{-                                                                    -}
{-    * * *  TEST QUESTIONS  * * *                                    -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 2pm on Monday 21st
   October.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   30% of the overall marks for the class (so one mark is worth half a
   percent).

   This file contains the test questions. Answer the questions in this
   file, and make sure that both are committed to GitLab both by the
   end of the lab session. -}

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
  chunkSize > 0 ==> False -- replace "False" here with a real property

{- 2 MARKS -}

{- 1.1.9 Removing Empties.

   The function 'splitOn' that you wrote in the take home part
   generates empty lists when there are consecutive occurrences of the
   splitting value. In some cases, we don't care about the empty
   lists. Write a *recursive* function that takes a list of lists, and
   returns a list only containing the non-empty elements in the same
   order. Examples:

      removeEmpty [[1],[],[2]]         == [[1],[2]]
      removeEmpty [[],[]]              == []
      removeEmpty ["hello","","world"] == ["hello","world"]

   Do not use 'filter' to write your function. -}

removeEmpties :: [[a]] -> [[a]]
removeEmpties = undefined

{- 2 MARKS -}

{- 1.1.10 Splitting into words.

   Using 'splitOn' and 'removeEmpties', write a function that splits a
   string into words, where words are separated by spaces (' ') and
   are not empty. Examples:

      words "hello world"   == ["hello","world"]
      words "hello   world" == ["hello", "world"]
      words ""              == []
      words "   "           == []
      words "hello, world"  == ["hello,", "world"] -}

words :: String -> [String]
words = undefined

{- 1 MARK -}

{----------------------------------------------------------------------}
{- PART II : CURSORS                                                  -}
{----------------------------------------------------------------------}

{- 1.2.6 Deletion.

   Write a function that deletes the element under the cursor (similar
   to pressing the 'delete' key in a text editor (not the backspace
   key!)). If there is no element under the cursor, then nothing
   happens. If there is any element to the right of the cursor, it is
   used to fill in the gap left. Examples:

     delete (AtEnd "cba")          == AtEnd "cba"
     delete (Within "cba" 'd' "")  == AtEnd "cba"
     delete (Within "cba" 'd' "e") == Within "cba" 'e' ""
-}

delete :: Cursor -> Cursor
delete cursor = undefined

{- SCHEME: 2 marks for a working definition. Mark off for getting the
   'AtEnd' case wrong. Nothing for implementing backspace. -}

{- 2 MARKS -}

{- 1.2.7 Undo.

   In this question, you will implement an undo feature for the
   cursor-based text editor implemented in the exercise. The structure
   used for implementing undo is very similar to the structure used
   for implementing the cursor.

   Here is the 'Undoable' type: -}

data Undoable
  = Undoable [Cursor] Cursor [Cursor]
  deriving Show


{- An 'Undoable' value has three parts:

       Undoable past present future

   It consists of a list holding the past cursors (in reverse), a
   single cursor for the present, and a list of cursors for the future
   (used for implementing 'redo'). Notice the similarity to the
   'Within' constructor in the 'Cursor' datatype. There is no analogue
   of the 'AtEnd' constructor, because it is not possible to be off
   the end of time.

   Implement the following functions.

   'now' returns the present cursor.

   'update' moves the 'present' to the past, puts the 'new' in the
   present, and erases the future.

   'undo' moves the head of the past to the present, and the present
   to the future. If there is no past, it returns 'Nothing'.

   'redo' moves the head of the future to the present, and the present
   to the past. If there is no future, it returns 'Nothing'. -}

now :: Undoable -> Cursor
now (Undoable past present future) = undefined -- Fill this in

update :: Undoable -> Cursor -> Undoable
update = undefined

undo :: Undoable -> Maybe Undoable
undo = undefined

redo :: Undoable -> Maybe Undoable
redo = undefined

{- 6 MARKS -}


{----------------------------------------------------------------------}
{- PART 3 : REPRESENTING PROCESSES                                    -}
{----------------------------------------------------------------------}

{- 1.3.5 Duplication.

   Write a process that inputs one bit, and then outputs it
   *twice*. You should have:

      process duplicate [True]  == [True,True]
      process duplicate [False] == [False,False]
-}

duplicate :: Process
duplicate = undefined

{- 1 MARK -}


{- 1.3.6 Flipping ouputs.

   Write a function that rewrites processes so that whenever the
   original process outputs 'True' the rewritten process outputs
   'False', and similarly for 'False'->'True'. -}

flipOutputs :: Process -> Process
flipOutputs = undefined

{- For example,

     process (flipOutputs (outputs [True,False,True])) []  == [False,True,False]
-}

{- 2 MARKS -}


{- 1.3.7 Flipping inputs.

   Write a function that rewrites processes to make them think that
   whenever they input 'True', they actually input 'False', and vice
   versa. -}

flipInputs :: Process -> Process
flipInputs = undefined

{- For example,

     process (flipInputs copyCat) [True]  == [False]
     process (flipInputs copyCat) [False] == [True]
-}

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE  (TEST QUESTIONS)                                  -}
{----------------------------------------------------------------------}
