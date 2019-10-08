{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex1 where

import Prelude hiding (words)
import Test.QuickCheck (quickCheck, (==>), Property)

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 1 : FIRST-ORDER PROGRAMMING               -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 2pm on Monday 21st
   October.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   30% of the overall marks for the class (so one mark is worth half a
   percent).

   The test will consist of another file which will import this
   file. You will need to answer the questions in that file, and
   commit both by the end of the lab session.


   Note about plagiarism: For the take home parts of this exercise,
   you can discuss the questions with others to make sure that you
   understand the questions. However, you must write up your answers
   by yourself. For the lab tests, no conferring is
   allowed. Plagiarism will be taken very seriously. -}

{----------------------------------------------------------------------}
{- PART 1 : FUN(ctional) WITH LISTS AND TREES                         -}
{----------------------------------------------------------------------}

{- 1.1.0 Concatenation of lists. The infix operator ++ concatenates
   (appends) two lists. Use it to write a function in pattern matching
   style which concatenates a list of lists. We have given you an
   unfinished definition which you should refine into suitable cases
   and complete. -}

concatLists :: [[x]] -> [x]
concatLists = undefined

{- It may help to think concretely:

   (a) What should
           concatLists [[1], [2,3], [4,5,6]]
       be?

   (b) What should
           concatLists [[2,3], [4,5,6]]
       be?

   (c) How do you combine the list '[1]' with the answer to (b) to
   make the answer to (a)? Remember that '[[1], [2,3], [4,5,6]]' is
   syntactic sugar for '[1]:[2,3]:[4,5,6]:[]'. -}

{- 2 MARKS -}

{- 1.1.1 Cons-ing an element to every list in a list of lists.

   The cons constructor ':' takes an element 'x' and a list 'xs', and
   creates a new list 'x:xs' with 'x' at the head and 'xs' at the
   tail.

   Write a function that performs this operation to a list of
   lists. For example:

        consAll 1 [[2,3],[4,5]] == [[1,2,3],[1,4,5]]
-}

consAll :: a -> [[a]] -> [[a]]
consAll = undefined

{- 2 MARKS -}

{- 1.1.2 Joining Lists. The 'joinWith' function takes a value 'x' and a
   list of lists 'xs' and concatenates the lists with 'x' placed between
   each list. Examples:

    joinWith 0 [[],[]]       == [0]
    joinWith 0 [[],[],[]]    == [0,0]
    joinWith 0 [[1,2],[3,4]] == [1,2,0,3,4]
    joinWith False [[True,True],[True],[True]] == [True,True,False,True,False,True]

   Note that the joining value is placed *between* consecutive
   elements, not at the ends. Also, because strings are lists of 'Char's,
   this function is also useful for building strings:

    joinWith ':' ["Ty Per", "ty.per@example.com"] == "Ty Per:ty.per@example.com"

   Write 'joinWith': -}

joinWith :: a -> [[a]] -> [a]
joinWith = undefined

{- 2 MARKS -}

{- 1.1.3 Splitting Lists. The function 'splitOn' splits a list at every
   occurence of some value. Examples:

      splitOn 0 [1,2,0,3,0]   == [[1,2],[3],[]]
      splitOn 0 [1,2,0,3,0,4] == [[1,2],[3],[4]]
      splitOn 0 []            == [[]]
      splitOn 0 [0]           == [[],[]]
      splitOn 0 [0,0]         == [[],[],[]]

   Because strings are lists of 'Char's, 'splitOn' is a useful way of
   breaking down strings:

       splitOn ':' "Ty Per:ty.per@example.com" == ["Ty Per", "ty.per@example.com"]

   Write the function 'splitOnHelper' that 'splitOn' uses to work. -}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitChar xs = splitOnHelper splitChar [] xs

splitOnHelper :: Eq a => a -> [a] -> [a] -> [[a]]
splitOnHelper = undefined

{- HINT: 'splitOnHelper' works by gathering elements of the input list
   'xs' in 'groups' until it sees an occurence of the splitting
   value. It then returns the current group (reversed back to normal),
   and carries on with a new group. When it runs out of elements to
   process, the last group is reversed and outputted.

   The arguments to 'splitOnHelper' are:

      splitOnHelper s group xs

      - 's :: a'       is the value to split on
      - 'group :: [a]' is the list of values in the current group, in reverse order
      - 'xs :: [a]'    is the input list of elements remaining to be processed. -}

{- 3 MARKS -}

{- 1.1.4 A property linking 'joinWith' and 'splitOn'.

   Here is a plausible property linking 'splitOn' and 'joinWith': if
   we join a list of strings with a character, and then split the
   result on that character, then we get back the original list of
   strings: -}

badProp :: Char -> [String] -> Bool
badProp c xs = splitOn c (joinWith c xs) == xs

{- Even though this property is plausible, it doesn't hold. Try it with
   'quickCheck' to find a counterexample where it fails.

   Write another property involving 'joinWith' and 'splitOn' that does
   hold. Test your property with quickcheck. (Note that this property
   must not be trivial -- it must be something that applies 'joinWith'
   and 'splitOn'. If it is possible to replace these functions in your
   property and still have it hold, then it is not "interesting"
   enough to get marks.)-}

goodProp :: Char -> String -> Bool
goodProp = undefined

{- 3 MARKS -}



{- 1.1.5 Elements of a tree.

   Here is a datatype of binary trees, with data of type 'a' stored at
   the nodes: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Write a function that returns all the elements in a tree, in left to
   right order: -}

elements :: Tree a -> [a]
elements = undefined

{- Examples:

         elements Leaf = []
         elements (Node (Node Leaf 1 Leaf) 2 Leaf) = [1,2]
-}

{- 2 MARKS -}


{- 1.1.6 Finding things in trees.

   Your mission is to test if a given value is somewhere in a tree,
   assuming that you can test values for equality. Write a function to
   test if a given value is in a given tree. You may want to use the
   "or" operator which is written || and applies to a pair of Bool
   values.  The "Eq x =>" bit in the type below means "assuming you
   can test elements of x for equality with the == operator" -}

eqFindInTree :: Eq a => a -> Tree a -> Bool
eqFindInTree = undefined

{- 3 MARKS -}


{----------------------------------------------------------------------}
{- PART II : CURSORS                                                  -}
{----------------------------------------------------------------------}

{- In this part of the exercise, you will implement a simple line
   editor. This will act a little like the editor you use on the Linux
   command line. You can also think of it as being like a single line
   in a text editor.

   The following datatype represents a 'pointer' into, or 'cursor' in,
   a list. It allows us to edit the middle of the list within having
   to search all the way down from the head each time. In this
   section, we will slowly build up to having a rudimentary line based
   text editor. -}

data Cursor
  = Within [Char] Char [Char]
  | AtEnd [Char]
  deriving Show

{- The 'Cursor' datatype has two constructors:

      'Within before point after'
        -- represents a cursor in the middle of the list
          - 'before' is the content of the list before the cursor, in reverse order
          - 'point'  is the item under the cursor
          - 'after'  is the content of the list after the cursor, in normal order

      'AtEnd before'
        -- represents a cursor just after the end of a list
          - 'before' is the content of the list before the cursor

   Examples:

      Within [2,1] 3 [4,5]  represents   [1,2, 3 ,4,5]
                                              ^^^

      Within [1] 2 [3,4,5]  represents   [1, 2 ,3,4,5]
                                            ^^^

      Within [] 1 [2,3,4,5] represents   [ 1 ,2,3,4,5]
                                          ^^^

      AtEnd [5,4,3,2,1]     represents   [1,2,3,4,5]
                                                    ^^^

   We will be particularly interested in cursors pointing into lists
   of characters. Because strings are lists of characters, they get
   special treatement in Haskell. Here are some examples, where the
   location of the cursor is represented by square brackets [-].

      Within "eh" 'l' "lo"  represents he[l]lo
      AtEnd "olleh"         represents hello[_]
      Within "" 'h' "ello"  repersents [h]ello

   The function 'displayCursor' gives an ASCII art rendering of a
   cursor over characters. Try it out in GHCi to see how it
   represents different cursor positions, and to get yourself
   familiar with the cursor representation. -}

revAppend :: [a] -> [a] -> [a]
revAppend []     ys = ys
revAppend (x:xs) ys = revAppend xs (x:ys)

displayCursor :: Cursor -> String
displayCursor (AtEnd before)              = revAppend before "[_]"
displayCursor (Within before point after) = revAppend before ('[':point:']':after)

{- 'toCursor' converts a list into a cursor, placing the cursor at the
   start of the list. Try it out in GHCi, along with the displayCursor
   function. -}

toCursor :: String -> Cursor
toCursor []     = AtEnd []
toCursor (x:xs) = Within [] x xs

{- 'fromCursor' forgets the position of the cursor in a list and returns
   the list. Again, try this function out in GHCi to familiarise
   yourself with the cursor representation. Try lots of different
   examples. -}

fromCursor :: Cursor -> String
fromCursor (AtEnd before)              = revAppend before []
fromCursor (Within before point after) = revAppend before (point:after)

{- 'getPoint' reads the item currently under the cursor. If the cursor
   is at the end of the line, it returns 'Nothing'. Otherwise, it
   returns 'Just point'. -}

getPoint :: Cursor -> Maybe Char
getPoint (AtEnd _)          = Nothing
getPoint (Within _ point _) = Just point


{- 1.2.0 Querying a cursor. Write two functions that (a) test whether or
   not a cursor is at the end of the line; and (b) test whether or not
   a cursor is at the start of the line. -}

atEnd :: Cursor -> Bool
atEnd = undefined

atStart :: Cursor -> Bool
atStart = undefined

{- 2 MARKS -}

{- 1.2.1 Movement. Here is a function that moves the cursor one step to
   the right. Note that there are three cases:

     1. If we are already at the end ('AtEnd'), we do nothing and
        return the cursor as is.

     2. If we are one before the end (the 'after' list is empty), the
        cursor becomes 'AtEnd', moving the point into the head of the
        'before' list.

     3. If the cursor is in the middle, we move the current point into
        the 'before' list, and take the head of the 'after' list as
        the new point.

   This definition illustrates why I chose to represent the 'before'
   list in reverse: it makes moving the cursor into a quick operation
   of prepending elements on to lists. -}

moveRight :: Cursor -> Cursor
moveRight (AtEnd before)                  = AtEnd before
moveRight (Within before point [])        = AtEnd (point:before)
moveRight (Within before point (a:after)) = Within (point:before) a after

{- Have a play with this function in GHCi to experiment with how it
   works.

   Now you write the 'moveLeft' function. There will be four cases:

    1. The cursor is at the end and start of an empty line
    2. The cursor is at the end of a non-empty line
    3. The cursor is at the start of a non-empty line
    4. The cursor is within a non-empty line.

   Turn these cases into Haskell patterns and work out what to do in
   each case.

   Some examples:

     moveLeft (AtEnd [])              == AtEnd []
     moveLeft (AtEnd "olleh")         == Within "lleh" 'o' ""
     moveLeft (Within "" 'h' "ello")  == Within [] 'h' "ello"
     moveLeft (Within "eh" 'l' "lo")  == Within "h" 'e' "llo"

   A helpful thing to remember is that moveLeft (like moveRight)
   should not alter the content of the cursor in any way. More
   formally, for all cursors 'c', 'fromCursor c == fromCursor
   (moveLeft c)'. -}

moveLeft :: Cursor -> Cursor
moveLeft = undefined

{- 2 MARKS -}


{- 1.2.2 Inserting Text. 'moveRight' and 'moveLeft' do not alter the
   content of the cursor. Now you will write a function that does edit
   the text. 'insert x cur' should insert the value 'x' "before" the
   cursor (in a similar way to pressing a key inserts a character
   "before" the cursor in your text editor). Examples:

      insert 5 (AtEnd [3,2,1])      == AtEnd [5,3,2,1]
      insert 5 (Within [2,1] 3 [4]) == Within [5,2,1] 3 [4]
-}

insert :: Char -> Cursor -> Cursor
insert = undefined

{- 1 MARK -}


{- 1.2.3 Backspace.

   Write a function that edits the cursor in the same way as your
   backspace key does. That is, it removes the character to the left
   of the cursor. Remember to think carefully about the possible edge
   cases. You may want to experiment with the backspace key in your
   text editor. Be careful not to delete the rest of your answers! -}

backspace :: Cursor -> Cursor
backspace = undefined

{- 2 MARKS -}


{- 1.2.4 Searching.

   Using 'getPoint' and 'moveRight', implement a recursive function
   that moves the cursor right until it finds a character that matches
   the given one. -}

searchRightFor :: Char -> Cursor -> Cursor
searchRightFor = undefined

{- 2 MARKS -}

{- 1.2.5 Macros = Lists of Instructions.

   The following datatype has a constructor for each of the different
   operations we have defined on cursors. When an operation has an
   argument (i.e., 'insert' and 'searchRightFor'), then the
   constructor has an argument. -}

data Instruction
  = MoveRight
  | MoveLeft
  | Insert Char
  | Backspace
  | SearchRightFor Char
  deriving Show

{- Write a function that takes a list of instructions, and performs them
   one by one on the given cursor. For example,

         execute [MoveRight, Insert 'x'] cursor

   should do the same as

         insert 'x' (moveRight cursor)

   If the list of instructions is empty, then the cursor should be
   returned, unmodified. -}

execute :: [Instruction] -> Cursor -> Cursor
execute = undefined

{- 3 MARKS -}

{- Once you have some or all of the functions above written, you will be
   able to use them as a simple text editor. Running

     λ> editor "Hello"
     [H]ello

   starts the editor and displays a cursor. Commands are entered by
   typing them and pressing 'Enter'. The commands are:

      'q'  -- quits
      'r'  -- move right
      'l'  -- move left (needs the moveLeft function written)
      'iX' -- inserts 'X' (needs the 'insert' function)
      'x'  -- removes the character directly to the left of the cursor (needs 'backspace')

    An example:

      λ> editor "Hel;o"
      [H]el;o
      r
      H[e]l;o
      r
      He[l];o
      r
      Hel[;]o
      x
      Hel[o]
      il
      Hell[o]
      q
      "Hello"
-}

data Result a
  = Continue a
  | Stop
  | Error
  deriving Show

decode :: String -> Cursor -> Result Cursor
decode "q"        cursor = Stop
decode "l"        cursor = Continue (moveLeft cursor)
decode "r"        cursor = Continue (moveRight cursor)
decode ['i',c]    cursor = Continue (insert c cursor)
decode "x"        cursor = Continue (backspace cursor)
decode ['/',c]    cursor = Continue (searchRightFor c cursor)
decode _          cursor = Error

editor :: String -> IO String
editor string = displayLoop initialState
  where
    initialState = toCursor string

    displayLoop cursor = do
      putStrLn (displayCursor cursor)
      cmd <- getLine
      case decode cmd cursor of
        Stop            -> do putStrLn ""; return (fromCursor cursor)
        Continue cursor -> displayLoop cursor
        Error           -> do putStrLn "???"; displayLoop cursor


{----------------------------------------------------------------------}
{- PART 3 : REPRESENTING PROCESSES                                    -}
{----------------------------------------------------------------------}

{- This exercise is about modelling processes which input and output
   bits. Processes are things. They're a kind of tree, representing a
   decision process, given by the following datatype. -}

{- We'll do the setup, then it'll be your turn. -}

data Process
  = End    -- marks the end of the process, so no more input or output
  | Output Bool Process
           -- (Output b p) outputs bit b, then continues as p
  | Input Process Process
           -- (Input pt pf) inputs a bit, continuing as pt if it's
           -- True, pf if False
  deriving Show

{- Don't expect the data in this type to *do* anything! Rather, they
   *represent* processes. We'll see how to interpret them shortly.

   Let's have an example process: this process should output False if its
   input is True and True if its input is False. -}

notGate :: Process
notGate = Input (Output False End) (Output True End)

{- See? If the input is True, we take the left path and find (Output
   False End), otherwise we go right and find (Output True End).
   Either way, we make one output and then stop.

   How can we make processes go? We need to interpret them. Here's
   how.  The "process" function takes a Process to interpret, and a
   list of input bits in [Bool], then produces the list of output
   bits. -}

process :: Process -> [Bool] -> [Bool]
process End            bs        = []
  -- when we're at the end, there is no more output
process (Output b p)   bs        = b : process p bs
  -- the output from (Output b p) had better begin with b, and the rest
  -- is whatever comes out from running p on the input
process (Input tp fp)  (b : bs)  = process (if b then tp else fp) bs
  -- when the process wants input, the result depends on the first bit
  -- in the input list: if that's True, then we continue with the tp
  -- branch; if it's false, we continue with the fp branch. In both
  -- cases, we feed the rest of the input bs to the continuing process
process (Input tp fp)  []        = []
  -- in the unfortunate case where the process wants input but the input
  -- list is empty, we're a bit stuck; let's stop and return no output

{- Let's try it out. Here are some test examples. Try loading this file
   in ghci, then evaluating testNotT and testNotF at the prompt. Do
   you get what you expect? -}

testNotT :: [Bool]
testNotT = process notGate [True]

testNotF :: [Bool]
testNotF = process notGate [False]

{- 1.3.0 Outputting a single bit. Write a function that takes a boolean
   value and returns a process that outputs that bit and ends. You
   should have:

      process (output True) [] == [True]

   and correspondingly for False. -}

output :: Bool -> Process
output = undefined

{- 1 MARK -}

{- 1.3.1 Copycat. Write a definition of a process, similar to the
   notGate, that reads its input and outputs it unaltered. You should
   have:

     process copyCat [True]   ==  [True]
     process copyCat [False]  ==  [False]
-}

copyCat :: Process
copyCat = undefined

{- 1 MARK -}

{- 1.3.2 Outputting multiple bits. Write a function that takes a list of
   bits and generates a process that outputs all of them, in
   order. You should have:

      process (outputs [True,False,True,True]) [] == [True, False, True, True]

   and so on. -}

outputs :: [Bool] -> Process
outputs = undefined

{- 2 MARKS -}

{- 1.3.3 Expectations. Write a function that given a list of bits, makes
   a process that reads that many bits from the input and outputs
   'True' if all the bits match the input list, and 'False'
   otherwise. You should have:

      process (expects [True])       [True]       == [True]
      process (expects [True, True]) [True,False] == [False]
      process (expects [True, True]) [True,True]  == [True]
      process (expects [])           [True,False] == [True]

   If 'expects' is given a list of length 'n', then it should always
   read exactly 'n' bits of input! Don't stop reading bits when you
   find a mismatch! -}

expects :: [Bool] -> Process
expects = undefined

{- 3 MARKS -}

{- 1.3.4 Sequencing processes. Write a function which combines two
   processes in sequence, so that the second begins once the first has
   ended.  That is, you should 'graft' the second process in place of
   all the End markers in the first process. HINT: the structure of
   this function is very similar to 'append'. -}

sequ :: Process -> Process -> Process
sequ = undefined

{- To check that you've got it right, make sure that

   process (sequ notGate notGate) [True,True]   = [False,False]
   process (sequ notGate notGate) [True,False]  = [False,True]
   process (sequ notGate notGate) [False,True]  = [True,False]
   process (sequ notGate notGate) [False,False] = [True,True]

   That is, sequencing two notGate components gives you a process
   which negates two inputs. -}


{- 3 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}
