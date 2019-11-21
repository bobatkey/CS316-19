module MonadsSolutions where

{-    MONADS

   These questions are about the material in Lectures 10-13 on monads,
   and simulating side effects using monads. -}


{- Q.0 Using 'Maybe' to evaluate expressions that may fail

   In Lecture 10, we saw how to use 'Maybe' to simulate programming
   with exceptions. In Lecture 12, we saw that 'Maybe' is a monad.

   Here is a datatype for expressions consisting of numbers 'Num <n>',
   addition of two 'Exp'ressions, and 'Abort', which aborts
   evaluation. You will write evaluators for 'Exp' that use 'Maybe' to
   simulate the effect of evaluating 'Abort' as throwing an exception. -}

data Exp
  = Num Int
  | Add Exp Exp
  | Abort
  deriving Show

{- (a) Write a function that evaluates 'Exp' to 'Maybe Int's:

     - evaluating 'Num <n>' should result in 'Just <n>'

     - evaluating an 'Add' expression should evaluate the two
       subexpressions and then, if they both return successfully, add
       the results. If either of the subexpressions fails, then the
       'Add' evaluates to 'Nothing'.

     - evaluating 'Abort' returns 'Nothing'.

   Write your function so that it does all the handling of failure
   explicitly, using 'case's. -}

eval :: Exp -> Maybe Int
eval (Num i) = Just i
eval (Add e1 e2) =
  case eval e1 of
    Nothing -> Nothing
    Just i1 ->
      case eval e2 of
        Nothing -> Nothing
        Just i2 ->
          Just (i1 + i2)

{- (b) Rewrite your 'eval' to use the 'Monad' interface for 'Maybe',
   using 'failure' to indicate failure of evaluation: -}

failure :: Maybe a
failure = Nothing

evalM :: Exp -> Maybe Int
evalM (Num i) = return i
evalM (Add e1 e2) =
  do x1 <- evalM e1
     x2 <- evalM e2
     return (x1 + x2)
evalM Abort = failure

{- (c) Rewrite 'eval' to use the 'Applicative' interface (Lecture 16)
   for 'Maybe' (remember that every 'Monad' is an 'Applicative') -}

evalA :: Exp -> Maybe Int
evalA (Num i)     = pure i
evalA (Add e1 e2) = pure (+) <*> evalA e1 <*> evalA e2
evalA Abort       = failure


{- Q.1 The 'Reader' monad.

   Here is another monad that is a bit like the 'State' monad from
   Lectures 11 and 12, except that there is no final state. It is
   usually called the 'Reader' monad because it is like having some
   global parameter that every part of the program can read. It is
   sometimes also called the 'Environment' Monad.

   Here, we will restrict to the case where the global parameter is an
   'Int'. -}

newtype Reader a = MkReader (Int -> a)

runReader :: Reader a -> Int -> a
runReader (MkReader f) = f

{- The 'Monad' implementation for Reader is this: -}

instance Monad Reader where
  {- 'return' ignores the global parameter and just returns the given
     value. -}
  return x = MkReader (\_ -> x)

  {- '>>=' runs the first operation, passing in the global parameter, then
     passes the result and the global parameter into the second
     operation: -}
  op >>= f = MkReader (\parameter ->
                          let a = runReader op parameter
                              b = runReader (f a) parameter
                          in b)

{- The Functor and Reader implementations use the standard definitions,
   from Lecture 16. -}
instance Functor Reader where
  fmap f r = pure f <*> r

instance Applicative Reader where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

{- Reader has a single operation that retrieves the value of the global
   parameter (this is like the 'get' operation for the State monad): -}

getParameter :: Reader Int
getParameter = MkReader (\parameter -> parameter)

{- Now write a function that takes a list of 'Maybe Int's and returns a
   list of 'Int's, using the 'Reader' monad to fill in the 'Nothing's
   with the global parameter:

   NOTE: there are several ways of writing this, using recursion using
   "do notation", using recursion using the 'Applicative' interface,
   and the other using 'mapM' or 'for' that we saw in Lecture 12. -}

fillInMissing :: [Maybe Int] -> Reader [Int]
fillInMissing = mapM (\x -> case x of
                              Nothing -> getParameter
                              Just i  -> return i)

fillInMissing_rec :: [Maybe Int] -> Reader [Int]
fillInMissing_rec [] = return []
fillInMissing_rec (x:xs) =
  do x' <- case x of
             Nothing -> getParameter
             Just i  -> return i
     xs' <- fillInMissing_rec xs
     return (x':xs')



{- Q.3 Parsing CSV files.

   In Lectures 14 and 15 we saw Parser Combinators, which are a useful
   interface for creating parsers.

   The implementation of 'Parser' that we wrote in Lecture 14 is at
   the bottom of this file. At the start of Lecture 15, we wrote a
   parser for string literals (i.e. strings in quotes, as they appear
   in programming languages). We decomposed the problem into writing a
   parser for string literal characters: -}

parseStringChar :: Parser Char
parseStringChar =
  do c <- parseChar
     case c of
       '"'  -> failParse
       '\\' -> parseChar
       c    -> return c

{- and a parser for string literals themselves: -}

parseStringLiteral :: Parser String
parseStringLiteral =
  do parseLiteralChar '"'
     cs <- zeroOrMore parseStringChar
     parseLiteralChar '"'
     return cs

{- We also wrote a generic parser for items separated by some parsable
   thing: -}

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator p =
  do x <- p
     xs <- zeroOrMore (do separator; p)
     return (x:xs)
  `orElse`
  return []

{- QUESTION: Write a parser for CSV files. A CSV file is:

    - a collection of records, separated by newlines '\n'

    - a record is a collection of fields, separated by commas

    - a field is either a string literal, or a raw string

    - raw strings cannot contain commas or newlines

  You may find it easier to write several smaller parsers for bits of
  the CSV format before writing the whole function. -}

parseRawString :: Parser String
parseRawString = zeroOrMore (do c <- parseChar
                                case c of
                                  '\n' -> failParse
                                  ','  -> failParse
                                  c    -> return c)

parseNewline :: Parser ()
parseNewline = parseLiteralChar '\n'

parseComma :: Parser ()
parseComma = parseLiteralChar ','

parseCSV :: Parser [[String]]
parseCSV =
  sepBy parseNewline
    (sepBy parseComma
       (parseStringLiteral `orElse` parseRawString))




{----------------------------------------------------------------------}
{- APPENDIX: Parser Combinators                                       -}
{----------------------------------------------------------------------}

newtype Parser a = MkParser (String -> Maybe (String, a))

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) input = p input

instance Monad Parser where
  return x = MkParser (\input -> Just (input, x))
  p >>= k =
    MkParser (\input ->
                case runParser p input of
                  Nothing ->
                    Nothing
                  Just (input2, a) ->
                    runParser (k a) input2)

parseChar :: Parser Char
parseChar =
  MkParser (\input -> case input of
                        c:input1 -> Just (input1, c)
                        _        -> Nothing)

failParse :: Parser a
failParse =
  MkParser (\input -> Nothing)

parseLiteralChar :: Char -> Parser ()
parseLiteralChar expected =
  do got <- parseChar
     if got == expected then return () else failParse

parseLiteral :: String -> Parser ()
parseLiteral = mapM_ parseLiteralChar

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Nothing -> runParser p2 input
                Just (rest,x) -> Just (rest,x))

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
   return []

instance Applicative Parser where
  pure      = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor Parser where
  fmap f p = pure f <*> p
