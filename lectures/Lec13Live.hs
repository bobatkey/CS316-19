module Lec13Live where

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
-}


{-
   1. 'Monad' is an interface with two functions for building side
      effecting operations:

      - Do nothing operation:
         'return :: a -> m a'

      - Sequence two operations:
         '>>=    :: m a -> (a -> m b) -> m b'

   2. We've seen several examples so far: 'Maybe', 'State',
      'Printing'.

   3. Haskell provides a special 'Monad' called 'IO' that represents
      "Input/Output actions".
-}


{- If we didn't use the 'IO' monad, then we'd have functions like:

      putChar :: Char -> ()

      void putChar(char c)

   but then we'd have:

      (1,1)

   has the same meaning as:

      let x = 1 in (x,x)

   but

      (putChar 'x', putChar 'x')

   can't be the same as

      let x = putChar 'x' in (x,x)

   because the first one prints 'x' twice, but the second one prints
   only once!
-}


{- Haskell gives 'putChar' the type:

     putChar :: Char -> IO ()

   So 'putChar 'x'' is an IO action that will print 'x'. It doesn't
   actually do any printing.

   Conceptually, 'IO' is similar to: -}

data IO' a
  = End a
  | Input (Char -> IO' a)
  | Output Char (IO' a)

{- (except that this isn't how it is implemented)

   IO actions are actually executed by either typing their name at the
   prompt in GHCi or by giving them the name 'main' in a standalone
   program. -}


{-    Part I : Output and Input -}


-- putChar
--    putChar :: Char -> IO ()

-- putTwoChars
putTwoChars :: Char -> Char -> IO ()
putTwoChars c1 c2 =
  do putChar c1
     putChar c2
  -- putChar c1 >>= \_ -> putChar c2 >>= \_ -> return ()

writeN :: Int -> Char -> IO ()
writeN n c =
  for_ [1..n] (\i -> putChar c)

printLine :: String -> IO ()
printLine str =
  do for_ str (\c -> putChar c)
     putChar '\n'
-- putStrLn


-- getChar
--    getChar :: IO Char

-- getTwoChars
getTwoChars :: IO (Char, Char)
getTwoChars =
  do c1 <- getChar
     c2 <- getChar
     return (c1, c2)

-- readLine
readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n' then return []
       else do cs <- readLine
               return (c:cs)

-- readLine2 (with an accumulator)
readLine2 :: IO String
readLine2 = go []
  where go accum =
          do c <- getChar
             if c == '\n' then return (reverse accum)
               else go (c:accum)

-- program
program :: IO ()
program =
  do printLine "Hello, what is your name?"
     name <- readLine
     printLine ("Hello " ++ name ++ "!")

-- capsLockSimulator
capsLockSimulator :: IO ()
capsLockSimulator =
  do line <- readLine
     if line == "" then return ()
       else do printLine (map toUpper line)
               capsLockSimulator


{-     Part II : "Real" mutable state -}

-- type IORef a

-- newIORef :: a -> IO (IORef a)

-- readIORef :: IORef a -> IO a          -- get

-- writeIORef :: IORef a -> a -> IO ()   -- put

-- ioRefExample
ioRefExample :: IO Int
ioRefExample =
  do cell <- newIORef 0
     writeIORef cell 45
     writeIORef cell 13
     value <- readIORef cell
     return value

-- modifyIORef :: IORef a -> (a -> a) -> IO ()

-- avg
avg :: [Double] -> IO Double
avg list =
  do sum <- newIORef 0
     len <- newIORef 0

     for_ list (\x -> do modifyIORef sum (\s -> s + x)
                         modifyIORef len (\l -> l + 1))

     finalSum <- readIORef sum
     finalLen <- readIORef len
     return (finalSum / finalLen)


{-    Part III : Input and Output to Files -}

-- FilePath, IOMode, Handle

--  type FilePath = String
--  data IOMode = ReadMode | WriteMode | ...
--  type Handle

-- openFile :: FilePath -> IOMode -> IO Handle

-- hPutChar :: Handle -> Char -> IO ()

-- hGetChar :: Handle -> IO Char

-- hIsEOF :: Handle -> IO Bool

writeToFile :: FilePath -> String -> IO ()
writeToFile path string =
  do handle <- openFile path WriteMode
     for_ string (\c -> hPutChar handle c)
     hClose handle

until_ :: IO Bool -> IO () -> IO ()
-- until_ :: Monad m => m Bool -> m a -> m ()
until_ cond body =
  do c <- cond
     if c then return ()
       else do body
               until_ cond body

readFromFile :: FilePath -> IO String
readFromFile path =
  do handle <- openFile path ReadMode
     buffer <- newIORef ""

     until_ (hIsEOF handle)
       (do c <- hGetChar handle
           modifyIORef buffer (\b -> c:b))

     hClose handle

     contents <- readIORef buffer
     return (reverse contents)


-- exceptions...

--    finally :: IO a -> IO b -> IO a

withInputFile :: FilePath -> (Handle -> IO a) -> IO a
withInputFile path body =
  do handle <- openFile path ReadMode
     result <- body handle `finally` hClose handle
        -- try { body (handle); } finally { handle.close(); }
     return result

readFromFile_v2 :: FilePath -> IO String
readFromFile_v2 path =
  withInputFile path
    (\handle ->
       do buffer <- newIORef ""

          until_ (hIsEOF handle)
          (do c <- hGetChar handle
              modifyIORef buffer (\b -> c:b))

          contents <- readIORef buffer
          return (reverse contents))
