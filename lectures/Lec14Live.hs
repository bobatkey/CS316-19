module Lec14Live where

import Data.Char     (isDigit, digitToInt)

{-      Lecture 14 : PARSER COMBINATORS I

   This lecture and the next are about writing parsers.


   Parsers turn "flat" sequences of characters (or, more low level,
   bytes):

      "(1+1)*2"

   which is really a sequence of characters:

       '(', '1', '+', '1', ')', '*', '2'

   into structured representations like:

       Mul (Add (Num 1) (Num 1)) (Num 2)
-}


{- Almost every program does some parsing:

     - Configuration files

     - Input data (CSV, JSON, XML, JPG, PNG, ...)

     - Network protocols (HTTP, SMTP, DNS, ...)

     - Compilers and interpreters parse programs

   Often, there's a standardised library to do it for you, but
   occasionally you'll need to write your own. -}




{-      Part I : Writing Parsers, a first attempt

   How should we write parsers in Haskell? One way to go about this is
   to have the following two thoughts:

   - Parsers take input that is a list of 'Char's, i.e., a 'String'.

   - Parsers can either fail to parse, because it recieved invalid
     input, or succeed, in which case it should return the structured
     representation.

   Thinking like this leads to the following definition ('version 1'): -}

type Parser_v1 a = String -> Maybe a


-- parseBool_v1
parseBool_v1 :: Parser_v1 Bool
parseBool_v1 "True"  = Just True
parseBool_v1 "False" = Just False
parseBool_v1 _       = Nothing



-- How to parse *two* booleans?
--
--    TrueTrue
--
--   (True, True)
--
--    TrueFalse
--
--   (True, False)




-- How can we fix this?



{-      Part II : Parsing with leftovers -}


-- The Parser type
newtype Parser a = MkParser (String -> Maybe (String, a))

-- runParser
runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) input = p input

-- parseBool_v2
parseBool_v2 :: Parser Bool
parseBool_v2 =
  MkParser (\input ->
              case input of
                'T':'r':'u':'e':rest     -> Just (rest, True)
                'F':'a':'l':'s':'e':rest -> Just (rest, False)
                _                        -> Nothing)

-- parsePair
parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Nothing -> Nothing
                Just (input2, a) ->
                  case runParser p2 input2 of
                    Nothing -> Nothing
                    Just (input3, b) ->
                      Just (input3, (a,b)))


-- parsePairOfBools


-- Monad?
instance Monad Parser where
  -- return :: a -> Parser a
  return a = MkParser (\input -> Just (input, a))

  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p1 >>= k =
    MkParser (\input ->
                case runParser p1 input of
                  Nothing -> Nothing
                  Just (input2, a) ->
                    runParser (k a) input2)

-- parsePairOfBools_v2
parsePairOfBools_v2 :: Parser (Bool, Bool)
parsePairOfBools_v2 =
  do b1 <- parseBool_v2
     b2 <- parseBool_v2
     return (b1, b2)

{-       Part III : Building Parsers

   We'll build more complex parsers from simple ones. -}


-- parseChar
parseChar :: Parser Char
parseChar =
  MkParser (\input ->
              case input of
                c:rest -> Just (rest, c)
                ""     -> Nothing)

parseTwoChars :: Parser (Char, Char)
parseTwoChars =
  do c1 <- parseChar
     c2 <- parseChar
     return (c1, c2)

-- failParse
failParse :: Parser a
failParse = MkParser (\input -> Nothing)

-- parseLiteralChar
parseLiteralChar :: Char -> Parser ()
parseLiteralChar expected =
  do got <- parseChar
     if got == expected then
       return ()
     else
       failParse

-- parseLiteral
parseLiteral :: String -> Parser ()
parseLiteral string =
  mapM_ parseLiteralChar string

-- parseBool_v3
parseBool_v3 :: Parser Bool
parseBool_v3 =
  do c <- parseChar
     case c of
       'T' -> do parseLiteral "rue"
                 return True
       'F' -> do parseLiteral "alse"
                 return False
       _   -> failParse

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
               case runParser p1 input of
                 Nothing -> runParser p2 input
                 Just (rest, a) -> Just (rest, a))

parseBool :: Parser Bool
parseBool =
  do parseLiteral "True"
     return True
  `orElse`
  do parseLiteral "False"
     return False

-- orderedDemo
orderedDemo :: Parser String
orderedDemo =
  do parseLiteral "for"
     return "The keyword 'for'"
  `orElse`
  do parseLiteral "forty"
     return "The number 40"


{- Have the following basic 'Parser's and ways of combining them:

     1. The 'parseChar' 'Parser' for parsing single characters.

     2. The monad interface 'return' and '>>=' for parsers that do
        nothing and sequence two parsers.

     3. The 'failParse' parser that always fails.

     4. The 'orElse' parser that tries one parser, and if that fails
        tries another.

   With these basic parsers, we have built 'parseLiteral' that
   recognises literal strings.

   With these basic functions, we no longer have to write anything
   involving 'MkParser' directly! -}



-- zeroOrMore
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x  <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
  return []

-- <list-of-ps> ::= <p> <list-of-ps>
--                |   /* empty */


twoBoolLists :: Parser ([Bool],[Bool])
twoBoolLists =
  do bools1 <- zeroOrMore parseBool
     bools2 <- zeroOrMore parseBool
     return (bools1, bools2)


{-  Example: Parsing Integers

   Want to parse strings like

       "1", "100", "123", "-90"

   into their numerical representations:

       1, 100, 123, -90
-}

parseDigit :: Parser Int
parseDigit =
  do c <- parseChar
     case c of
       '0' -> return 0
       '1' -> return 1
       '2' -> return 2
       '3' -> return 3
       '4' -> return 4
       '5' -> return 5
       '6' -> return 6
       '7' -> return 7
       '8' -> return 8
       '9' -> return 9
       _   -> failParse

parsePositiveNumber :: Parser Int
parsePositiveNumber =
  do d  <- parseDigit
     ds <- zeroOrMore parseDigit
     return (foldl (\t d -> t*10 + d) d ds)

parseSign :: Parser Int
parseSign =
  do parseLiteralChar '-'
     return (-1)
  `orElse`
  return 1

parseInt :: Parser Int
parseInt =
  do sign <- parseSign
     number <- parsePositiveNumber
     return (sign * number)



-- parseInt


-- These two type class instances are needed for the 'Monad' instance
-- defined above to be allowed. They will be explained in Lecture 16.

instance Applicative Parser where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor Parser where
  fmap f p = pure f <*> p
