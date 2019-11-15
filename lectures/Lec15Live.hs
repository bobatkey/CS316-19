module Lec15Live where

import Data.Char (isAlpha, isNumber, isSpace)

{-     Lecture 15 : PARSER COMBINATORS II

   This lecture is a continuation of the previous one, so we... -}

import Lec14

{- And we get the parser combinators we had from last time. Remember
   that this consisted of:

     - A type constructor 'Parser a' of parsers of values of type 'a'

     - Implementations of the 'Monad' interface for 'Parser', so we
       have:

         1. A 'Parser' that consumes no input and returns a fixed value:

               return :: a -> Parser a

         2. A way of sequencing two 'Parser's, one after the other,
            using '>>=' ("bind"). We almost never write '>>=' by hand
            though, and just use the "do" notation.

     - 'parseChar' is a 'Parser Char' that consumes exactly one
       character from the input and returns it.

     - 'failParse' is a 'Parser a' that always fails. We use this if
       we want to reject some input that we've just read.

     - 'orElse' tries one parser, and if that fails, tries another
       one. We use this when the input may be one thing or another.

     - a function called 'runParser' for actually running a 'Parser'
       on some input.

   With these basic functions, we built a few useful extra bits, like
   'zeroOrMore', which parses zero or more repetitions of a
   'Parser'. We used this to build a parser for 'Int's, called
   'parseInt'. -}

{-     Part I : Parsing String Literals

   The last lecture finished with an exercise: to write a parser for
   string literals like they appear in most programming languages:

         "hello, I'm a string literal"

   String literals always end at the first '"'. To write string
   literals that represent strings containing '"'s you need to
   "escape" them by writing a '\' in front of them:

         "this is a \"string\" literal"
-}


parseStringChar :: Parser Char
parseStringChar =
  do c <- parseChar
     case c of
       '"'  -> failParse
       '\\' -> do c <- parseChar
                  return c
       c    -> return c

parseStringLiteral :: Parser String
parseStringLiteral =
  do parseLiteralChar '"'
     cs <- zeroOrMore parseStringChar
     parseLiteralChar '"'
     return cs



{-     Part II : Parsing JSON


       { "f": 1,
         "g": [ 1, null, true, "burp", false, [ "a", "aa", "aaa" ] ],
         "name": [ 4, null, 5, null, { "a" : 10 } ]
       }
-}

data JSON
  = Number    Int
  | Boolean   Bool
  | Null
  | StringLit String
  | Array     [JSON]
  | Object    [(String, JSON)]
  deriving Show

parseJSONBool :: Parser Bool
parseJSONBool =
  do parseLiteral "true"
     return True
  `orElse`
  do parseLiteral "false"
     return False

parseNull :: Parser ()
parseNull =
  do parseLiteral "null"
     return ()

{- The remaining kinds of JSON value to parse are arrays:

     [ 1, 2, null, false, "blarp" ]

   and objects:

     {"Aberdeen":5,"Celtic":0}
-}

parseSpace :: Parser ()
parseSpace =
  do c <- parseChar
     if isSpace c then return () else failParse

parseWhitespace :: Parser ()
parseWhitespace =
  do zeroOrMore parseSpace
     return ()

-- 1, 2, 3, 4

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator parseItem =
  do x  <- parseItem
     xs <- zeroOrMore (do separator
                          parseItem)
     return (x:xs)
  `orElse`
  return []

comma :: Parser ()
comma = parseLiteralChar ','

spacedComma :: Parser ()
spacedComma =
  do parseWhitespace
     comma
     parseWhitespace

parseList :: Parser a -> Parser [a]
parseList parseItem =
  do parseLiteralChar '['
     parseWhitespace
     items <- sepBy spacedComma parseItem
     parseWhitespace
     parseLiteralChar ']'
     return items


parseObjectItem :: Parser a -> Parser (String, a)
parseObjectItem parseItem =
  do fieldname <- parseStringLiteral
     parseWhitespace
     parseLiteralChar ':'
     parseWhitespace
     value <- parseItem
     return (fieldname, value)


parseObject :: Parser a -> Parser [(String,a)]
parseObject parseItem =
  do parseLiteralChar '{'
     parseWhitespace
     items <- sepBy spacedComma (parseObjectItem parseItem)
     parseWhitespace
     parseLiteralChar '}'
     return items


parseJSON :: Parser JSON
parseJSON =
  do num <- parseInt
     return (Number num)
  `orElse`
  do bool <- parseJSONBool
     return (Boolean bool)
  `orElse`
  do parseNull
     return Null
  `orElse`
  do string <- parseStringLiteral
     return (StringLit string)
  `orElse`
  do items <- parseList parseJSON
     return (Array items)
  `orElse`
  do fields <- parseObject parseJSON
     return (Object fields)

readJSON :: FilePath -> IO (Maybe JSON)
readJSON path =
  do contents <- readFile path
     case runParser parseJSON contents of
       Nothing        -> return Nothing
       Just ("",json) -> return (Just json)
       Just ("\n",json) -> return (Just json)
       Just (_,_)     -> return Nothing
