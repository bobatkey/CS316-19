module Main where

import Ex3
import System.Environment (getArgs, getProgName, withArgs)
import System.Exit (exitFailure)

{- The following main function makes it possible to compile this file
   and run your GHOUL evaluator from the command-line, reading a
   source program from a file name given as an argument. (Of course,
   this won't work until you have managed to get runGHOUL working!)

   You can compile your file as follows:

         $ ghc Ex3Main.hs

   This will produce an executable file named 'Ex3Main' in the current
   directory. To run it on a file plus.ghoul containing a GHOUL
   program, execute it like this:

         $ ./Ex3Main plus.ghoul
         (S (S (S (S Z))))

   You can also run the main function with a "faked" commandline
   argument from ghci directly, without compiling, by running

         λ> withArgs ["plus.ghoul"] main
   -}

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  ghoulFile <-
    case args of
      [f] -> return f
      _   -> exitFail ("not exactly one input file.\n" ++
                       "Usage: " ++ progName ++ " <input-file>")
  input <- readFile ghoulFile
  case runGHOUL input of
    Error err      -> exitFail err
    Ok (Error err) -> exitFail err
    Ok (Ok v)      -> putStrLn (ppValue v)
  where
    exitFail msg = do putStrLn ("GHOUL: " ++ msg)
                      exitFailure
