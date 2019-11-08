module Lec13 where

import Data.Char          (toUpper)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar,
                           hClose, IOMode (..), hIsEOF, Handle)

{-      Lecture 13 : INPUT / OUTPUT


   Since Lecture 10, we've seen how to simulate side effects in
   Haskell. But what if we really want to do some side effects?

   A great philosopher once wrote:

     The philosophers have only interpreted the world, in various
     ways. The point, however, is to change it.

      -- Karl Marx ( https://en.wikipedia.org/wiki/Theses_on_Feuerbach )

   So how do we change the world, but remain with a language that only
   allows functions with no side effects?

   The answer lies in the 'Monad' interface we've built up over the
   last three lectures. In each of the instances of 'Monad' we've seen
   so far, we've isolated the idea of "do some side effects and
   possibly return a value" into different types according to the
   different kinds of side effects. For each of the kinds of side
   effect we've seen so far, we've given a concrete implementation
   that simulates that side effect. But what if we had a special
   implementation of 'Monad' that was implemented in terms of real
   side effects on the world? The 'Monad' interface would then allow
   us to build abstract "actions" that perform side effects.

   To do real I/O, Haskell offers an abstract type 'IO' that
   implements the 'Monad' interface. Operations that would perform
   some real I/O in other languages become 'IO' actions in
   Haskell. For example, a function that prints a character from the
   input in a language with I/O everywhere would have the type:

       putChar :: Char -> ()

   But having such a function in Haskell would spoil our ability to
   reason by replacing equals by equals. For example, in the code:

      (putChar 'x', putChar 'x')

   We should be able to think: there is duplicated code here, we
   should be able to pull it out into a single definition:

      let x = putChar 'x'
      in (x,x)

   But now if we read 'putChar 'x'' as actually doing the side effect
   of outputting a character, we have two different programs. The
   first outputs two 'x's, and the second outputs only one.

   In Haskell, 'putChar' has the type:

       putChar :: Char -> IO ()

   That is, 'putChar' doesn't actually do the printing when you call
   it. Instead, it returns an "IO action". So the following code:

       (putChar 'x', putChar 'x')

   doesn't output two characters, instead it returns a pair of two IO
   actions, that, when they are executed, output 'x's. Because
   'putChar 'x'' is an IO action, not an instruction to output, the
   rewritten code:

       let x = putChar 'x'
       in (x,x)

   Has exactly the same meaning.

   Conceptually, we can think of such "actions" as being in a datatype
   similar to the 'Process' type from Exercise 3: -}

data IO' a
  = End a
  | Input (Char -> IO' a)
  | Output Char (IO' a)

{- If actions are just some datattype, we can pass around actions, and
   combine them in various ways, all while retaining the ability to
   reason about our programs using replacement of equals by
   equals. Only when an action is passed to the Haskell system does it
   actually get executed, and the effects seen.

   The real IO monad is not actually implemented like this. The main
   difference between 'IO'' and the real 'IO' monad is that we cannot
   pattern match on values of type 'IO a'. This allows the Haskell
   system to more efficiently optimise and execute IO actions.

   In summary, Haskell remains a "pure" language, and allows side
   effects by:

     1. Having a special type 'IO a' of I/O actions that compute
        values of type 'a'.

     2. Using the 'Monad' interface to combine individual 'IO a'
        actions into sequences.

     3. Actually executing an 'IO a' action happens either by typing
        its name at the prompt in GHCi, or in a standalone program by
        being whatever action is defined to be the 'main' value.

   In this lecture, we'll look at some of the basic operations that
   the IO monad has, and how they can be put together to write
   programs that interact with the outside world. -}


{-    Part I : Output and Input

   So 'IO a' is the type of "IO actions computing values of type
   'a'". But what are the primitive operations for 'IO'? The 'Maybe'
   monad has 'failure' and 'catch', the 'State' monad has 'get' and
   'put', and so on. What about 'IO'?

   'IO' has a lot of operations, corresponding to all the different
   ways of interacting with the outside world. Covering all of the
   them is well outside the scope of this course, so I'll just cover a
   few to get the general idea.

   Perhaps the simplest is 'putChar', which has the following type:

      putChar :: Char -> IO ()

   As the name indicates, it puts a single character to the output:

      > putChar 'x'
      x>

   Notice that there is no newline between the 'x' and the next prompt
   from GHCi.

   Since 'IO' is a 'Monad', we can use the "do notation" described in
   Lecture 12 with it, and print two characters, one after the other: -}

putTwoChars :: Char -> Char -> IO ()
putTwoChars c1 c2 =
  do putChar c1
     putChar c2

{- For example:

      > putTwoChars 'h' 's'
      hs>

   Using the generic 'for_' function from Lecture 12 that iterates
   some action for every element of a list, we can write a function
   that writes a character a fixed number of times to the output: -}

writeN :: Int -> Char -> IO ()
writeN n c = for_ [1..n] (\_ -> putChar c)

{- For example:

      > writeN 20 'a'
      aaaaaaaaaaaaaaaaaaaa>

   More usefully, we can write a function that iterates through a
   'String' (which is a list of characters), running putChar on each
   one, and then outputs a newline character: -}

printLine :: String -> IO ()
printLine xs =
  do for_ xs (\x -> putChar x)
     putChar '\n'

{- For example:

      > printLine "Hello world"
      Hello world

   This function is already in the standard library as 'putStrLn',
   which knows more about OS-specific line endings.

   There is also a primitive "read a character" operation:

      getChar :: IO Char

   From the type, 'getChar' is an IO operation that returns a
   'Char'. For example:

      > getChar
      x                   <-- this is entered by the user
      'x'                 <-- this is printed by GHCi

   Note that you'll have to press 'Enter' after typing a character for
   the terminal to actually send the character to GHCi.

   As with 'putChar', we can use the 'Monad' operations of 'IO' to
   sequence uses of 'getChar': -}

getTwoChars :: IO (Char, Char)
getTwoChars =
  do c1 <- getChar
     c2 <- getChar
     return (c1, c2)

{- For example:

      > getTwoChars
      xy                 <-- entered by user
      ('x','y')          <-- printed by GHCi

   (again, you'll need to press 'Enter' after the two characters to
   make the terminal actually send the input.)

   To read lines of input, we need to keep reading characters until we
   read a newline ('\n'). The following function does this: -}

readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n' then return []
     else do cs <- readLine
             return (c:cs)

{- The following function does the same thing, except that it uses an
   accumulator to build up the list of characters from the input, and
   reverses it at the end. This function consumes less stack space
   than 'readLine'. -}

readLine2 :: IO String
readLine2 = go []
  where go accum =
          do c <- getChar
             if c == '\n' then
               return (reverse accum)
             else
               go (c:accum)

{- Either way, you probably shouldn't implement this function yourself,
   because it is already in the standard library as 'getLine'. The
   standard library version also knows more about OS-specific line
   endings.

   Now that we have the ability to read and write whole lines, we can
   write simple functions that interact with the user. The classic
   "What is your name?" function: -}

program :: IO ()
program =
  do printLine "Hello, what is your name?"
     name <- readLine
     printLine ("Hello " ++ name ++ "!")

{- Example run:

     > program
     Hello, what is your name?
     Bob
     Hello Bob!

   A function that repeatedly reads input until it gets an empty line: -}

capsLockSimulator :: IO ()
capsLockSimulator =
  do line <- readLine
     if null line then return ()
       else do printLine (map toUpper line)
               capsLockSimulator

{- An example interaction:

     > capsLockSimulator
     hello
     HELLO
     no, really, i am being calm
     NO, REALLY, I AM BEING CALM

     >
-}


{-     Part II : "Real" mutable state

   In Lecture 11 we saw how to simulate mutable state in Haskell by
   passing the value of the state around and creating new variables
   each time we wanted to update the value.

   This simulation works, but one might quibble that computer hardware
   provides very good facilities for doing real mutable state, so why
   don't we use them?

   Via the 'IO' monad, we can use "real" mutable state, keeping our
   language pure. The interface is similar to the 'get', 'put'
   interface of the 'State' monad, except that we can have multiple
   mutable variables at the same time. The type of mutable variables
   is an abstract type (meaning that we don't get to see the
   definition):

       type IORef a

   So a value of type 'IORef a' is a reference to a mutable state cell
   containing a value of type 'a'.

   We allocate a new cell with:

       newIORef    :: a -> IO (IORef a)

   And we read and write them using the functions:

       readIORef   :: IORef a -> IO ()

       writeIORef  :: IORef a -> a -> IO ()

   which are analogous to the 'get' and 'put' functions from the
   'State' monad, extended with an extra argument saying which
   reference cell is being read or written to.

   Here is an example of their use that just creates a new reference
   cell, updates it twice, and returns the value: -}

ioRefExample :: IO Int
ioRefExample =
  do cell <- newIORef 0
     writeIORef cell 45
     writeIORef cell 23
     value <- readIORef cell
     return value

{- Running this returns the last value written to the cell, as we would
   expect:

       > ioRefExample
       23

   Often, we want to read a cell and immediately write it with an
   updated value. The following function takes a reference cell to
   modify and a function to modify it with, doing the read and write
   in one step:

       modifyIORef :: IORef a -> (a -> a) -> IO ()

   Here's an example of using it to compute the average of a list of
   'Double's (like the 'sumAndLen' question in Exercise 2), using a
   'for_' loop through the list and two variables, one to hold the
   sum, and one to hold the length: -}

avg :: [Double] -> IO Double
avg xs =
  do sum <- newIORef 0
     len <- newIORef 0

     for_ xs (\x -> do modifyIORef sum (\v -> v+x)
                       modifyIORef len (\v -> v+1))

     finalSum <- readIORef sum
     finalLen <- readIORef len
     return (finalSum / fromIntegral finalLen)

{- For example:

      > avg [4,5,6,7,8]
      6.0

   You might be wondering what 'IO' (i.e., Input / Output) has to do
   with mutable state. Why does the 'IO' monad offer facilities for
   mutable state? One answer is that mutable state is often useful
   when doing I/O. Another answer is that Haskell also has another
   built-in monad, called 'ST', for doing stateful computations that
   do not do any IO. -}


{-    Part III : Input and Output to Files

   The 'putChar' and 'getChar' functions above allow us to read from
   the standard input and write to the standard output. But what if we
   want to read and write to specific files?

   Just as in many other languages, Haskell provides facilities for
   opening files to get file handles, which can be given to read and
   write function, and then finally closed. The basic API for this is
   as follows:

   File paths are represented as 'String's, using a type synonym:

       type FilePath = String

   When we open files, we have to say what we're going to do with it:
   read, write, append to the end, or random access reading and
   writing:

       data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

   After we open a file, we get back a 'Handle', which is another
   abstract type:

       type Handle

   The function to actually open files is 'openFile' which takes a
   'FilePath' and an 'IOMode' and returns an 'IO' action that will
   yield a 'Handle' (or throw an exception if it can't open the file,
   see below):

       openFile :: FilePath -> IOMode -> IO Handle

   To read and write individual characters from a 'Handle', we use
   variants of the 'getChar' and 'putChar' functions we saw above:

       hGetChar :: Handle -> IO Char

       hPutChar :: Handle -> Char -> IO ()

   Finally, when we are done with a file, we 'hClose' the 'Handle' to
   return the associated resources to the operating system:

       hClose   :: Handle -> IO ()

   Let's see those functions in action. Here is a function that writes
   a 'String' to a file (called 'writeFile' in the standard
   library). It opens the specified file in 'ReadMode', uses a 'for_'
   loop to write every character from 'string' into the handle, and
   then closes the handle. -}

writeToFile :: FilePath -> String -> IO ()
writeToFile path string =
  do handle <- openFile path WriteMode
     for_ string (\x -> hPutChar handle x)
     hClose handle

{- Note that this function is not "exception safe": if 'hPutChar' throws
   an exception (e.g., the disk is full), then the file won't be
   closed properly. We'll see how to fix this below.

   To read an entire file into a 'String', we'll need to keep reading
   until the end of the file. To find out if a 'Handle' is at the end
   of a file (EOF), we can use the function 'hIsEOF':

      hIsEOF :: Handle -> IO Bool

   We could write another recursive function to read all the input
   until EOF, just as we did for 'readLine' above. Another possibility
   is to use the fact that, in Haskell, IO actions are first-class
   entities in the language. That is, a value of type 'IO a' is a
   representation of an IO action that we can pass around between
   functions. A consequence of this is that we can write our own
   control structures, such as custom loops and conditionals. We've
   already seen an example of this with 'mapM_' and 'for_' in Lecture
   12.

   Since we want to keep reading until EOF, we need some kind of
   'until' loop, which is a bit like a 'while' loop except that it
   keeps executing *until* the condition is 'True'.

   Here is the implementation, which is completely generic in the
   monad 'm' (i.e., it is not specific to IO): -}

until_ :: Monad m => m Bool -> m () -> m ()
until_ cond body =
  do b <- cond
     if b then
        return ()
     else
        do body
           until_ cond body

{- If we have an action 'cond' that returns a boolean, representing the
   loop condition, and an action 'body' that is to be executed until
   the condition is 'True', then we can construct an action that does
   this like so:

      until_ cond body
-}

{- EXERCISE: how would you write 'while_'? -}

{- Using our new 'until_' loop, we can write a function that reads a
   whole file into a string by:

    1) opening the file

    2) creating a new reference cell holding the empty string

    3) until EOF, reading a character from the file and prepending it
       on to the string stored in the reference cell

    4) closing the file

    5) reading out the list of characters, reversing and returning it
-}

readFromFile :: FilePath -> IO String
readFromFile path =
  do handle <- openFile path ReadMode

     content <- newIORef ""

     until_ (hIsEOF handle)
       (do c <- hGetChar handle
           modifyIORef content (\cs -> c:cs))

     hClose handle

     cs <- readIORef content
     return (reverse cs)

{- Above, I mentioned that these functions are not "exception safe" in
   the sense that if one of the I/O operations throws an exception,
   then any open files will not be close properly, and there will be a
   resource leak.

   A proper discussion of handling of IO exceptions in Haskell
   (distinct from the 'simulated' exceptions we saw in Lecture 10) is
   beyond the scope of this course. However, I'll end this note with
   how to fix this problem.

   Haskell provides a function 'finally' that is similar to the
   'finally' blocks in Java. It has the type:

       finally  :: IO a -> IO b -> IO a

   The idea is that

       finally action cleanup

   Is an IO action that performs 'action'. Then, however 'action'
   terminates, either normally, or by throwing an exception, 'cleanup'
   is performed. The whole action then either terminates with a value
   or rethrows the exception.

   We can use finally to write a higher order function that opens a
   file and then passes the handle to the given 'body', using
   'finally' to close the file no matter how 'body' terminates. -}

withInputFile :: FilePath -> (Handle -> IO a) -> IO a
withInputFile path body =
  do handle <- openFile path ReadMode
     result <- body handle `finally` hClose handle
     return result

{- With this function, we can rewrite 'readFromFile' to be exception
   safe. Now, if 'hIsEOF' or 'hGetChar' throw an exception, the file
   will still be safely closed. -}

readFromFile_v2 :: FilePath -> IO String
readFromFile_v2 path =
  withInputFile path (\handle ->
    do content <- newIORef ""

       until_ (hIsEOF handle)
         (do c <- hGetChar handle
             modifyIORef content (\cs -> c:cs))

       cs <- readIORef content

       return (reverse cs))

{- EXERCISE: Write a function 'withOutputFile' that does what
   'withInputFile' does but for output. Use it to write an exception
   safe version of 'writeToFile' -}
