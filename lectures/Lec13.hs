module Lec13 where

import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Concurrent (forkIO, newMVar, readMVar, putMVar)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar, hClose, IOMode (..), hIsEOF, Handle)

{-      Lecture 13 : INPUT / OUTPUT


   Since Lecture 10, we've seen how to simulate side effects in
   Haskell. But what if we really want to do some side effects?

   A famous philosopher once wrote:

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

   In summary, Haskell remains a "pure" language, and allows side
   effects by:

     1. Having a special type 'IO a' of I/O actions that return values
        of type 'a'.

     2. Using the 'Monad' interface to combine individual 'IO a'
        actions into sequences.

     3. Actually running an 'IO a' action happens either by typing its
        name at the prompt in GHCi, or in a standalone program by
        being whatever action is defined to be the 'main' value.

   In this lecture, we'll look at some of the basic operations that
   the IO monad has, and how they can be put together to write
   interesting programs that interact with the outside world. -}


{-    Part I : Input and Output -}

{- putChar :: Char -> IO () -}

putTwoChars :: Char -> Char -> IO ()
putTwoChars c1 c2 =
  do putChar c1
     putChar c2

writeN :: Int -> Char -> IO ()
writeN n c = for_ [1..n] (\_ -> putChar c)

printLine :: String -> IO ()
printLine xs =
  do for_ xs (\x -> putChar x)
     putChar '\n'

{- In the standard library as 'putStrLn', which also knows more about
   OS-specific line endings. -}


{- getChar :: IO Char -}

getTwoChars :: IO (Char, Char)
getTwoChars =
  do c1 <- getChar
     c2 <- getChar
     return (c1, c2)


readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n' then return []
     else do cs <- readLine
             return (c:cs)

{- In the standard library as 'getLine', which handles  -}

{- An example program: -}

program :: IO ()
program =
  do printLine "Hello, what is your name?"
     name <- readLine
     printLine ("Hello " ++ name ++ "!")


{-    Part II : "Real" mutable state


   newIORef :: a -> IO (IORef a)

   writeIORef :: IORef a -> a -> IO ()

   readIORef :: IORef a -> IO ()

   modifyIORef :: IORef a -> (a -> a) -> IO ()
-}

sumAndAvg :: [Double] -> IO (Double, Integer)
sumAndAvg xs =
  do sum <- newIORef 0
     len <- newIORef 0
     for_ xs (\x -> do modifyIORef sum (\v -> v+x)
                       modifyIORef len (\v -> v+1))
     finalSum <- readIORef sum
     finalLen <- readIORef len
     return (finalSum, finalLen)



{-    Part III : Input and Output to Files


   type FilePath = String

   openFile :: FilePath -> IOMode -> IO Handle

   hGetChar :: Handle -> IO Char

   hPutChar :: Handle -> Char -> IO ()

   hClose   :: Handle -> IO ()
-}

writeToFile :: FilePath -> String -> IO ()
writeToFile path string =
  do handle <- openFile path WriteMode
     for_ string (\x -> hPutChar handle x)
     hClose handle

until_ :: IO Bool -> IO () -> IO ()
until_ cond body =
  do b <- cond
     if b then return ()
       else do body
               until_ cond body

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

{-
   finally  :: IO a -> IO b -> IO a
-}

withInputFile :: FilePath -> (Handle -> IO a) -> IO a
withInputFile path k =
  do handle <- openFile path ReadMode
     result <- k handle `finally` hClose handle
     return result

readFromFile_v2 :: FilePath -> IO String
readFromFile_v2 path =
  withInputFile path (\handle ->
    do content <- newIORef ""
       until_ (hIsEOF handle)
         (do c <- hGetChar handle
             modifyIORef content (\cs -> c:cs))
       cs <- readIORef content
       return (reverse cs))
