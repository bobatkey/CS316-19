module Lec15 where

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

   So how do we do this? The first thing to do is to write a parser
   for the individual characters in a string literal (the bits between
   the '"' marks). Most characters stand for themselves, except when
   we see a bare '"', which cannot appear in a string literal, or a
   backslash '\', which indicates that the next character(s) are to be
   treated specially (e.g., "\n" for the newline character). For our
   simple parser, we'll allow a backslash followed by any character to
   stand for that character. Real string literal syntaxes allow for
   things like Unicode codepoints and so on.

   Here is the parser for individual characters, which implements the
   scheme above: -}

parseStringChar :: Parser Char
parseStringChar =
  do c <- parseChar
     case c of
       '"'  -> failParse
       '\\' -> do c <- parseChar
                  -- FIXME: this should be more sophisticated
                  -- see the JSON RFC
                  return c
       c    -> return c

{- Some examples (note that the '"' and '\' characters here have to be
   escaped for Haskell's own string literal syntax!):

       > runParser parseStringChar "abc"
       Just ("bc",'a')
       > runParser parseStringChar ""
       Nothing
       > runParser parseStringChar "\""
       Nothing
       > runParser parseStringChar "\\\""
       Just ("",'"')

   The last one demonstrates that by putting a backslash in front of a
   '"', we parse a '"'.

   To parse whole string literals, we first look for an opening '"',
   then zero or more string literal characters, and then a closing
   '"'. Note that 'parseStringChar' fails on '"'s, so the zeroOrMore
   parser will stop reading when it gets to the closing '"'. The code
   that implements this is as follows: -}

parseStringLiteral :: Parser String
parseStringLiteral =
  do parseLiteralChar '"'
     cs <- zeroOrMore parseStringChar
     parseLiteralChar '"'
     return cs

{- Some examples:

   1. Not a string literal:

       > runParser parseStringLiteral "hello"
       Nothing

   2. Missing a terminating '"':

       > runParser parseStringLiteral "\"hello"
       Nothing

   3. A complete string literal:

       > runParser parseStringLiteral "\"hello\""
       Just ("","hello")

   4. A string literal that terminates with more stuff after it:

       > runParser parseStringLiteral "\"hel\"lo\""
       Just ("lo\"","hel")

   5. A string literal with an escaped character:

       > runParser parseStringLiteral "\"hel\\\"lo\""
       Just ("","hel\"lo")
-}

{-     Part II : Parsing JSON

   Parsing numbers and string literals is all very fine, but does the
   'Parser' approach scale to more complex things? As a case study,
   let's look at parsing of JSON (the JavaScript Object Notation), a
   commonly used data exchange format. To keep things small, our
   parser won't handle all of JSON, but a reasonable subset.

   JSON is (nearly) a subset of JavaScript syntax for describing data
   that is either:

     - a number: real JSON allows floating point numbers, but we'll
       restrict to integers for now

     - booleans: either "true" or "false"

     - nulls: the literal string "null"

     - string literals: real JSON specifies a complex system of escape
       codes for putting arbitrary unicode codepoints in strings, but
       we'll stick to the string literal syntax we defined above.

     - arrays: sequences of JSON data, separated by commas and
       surrounded by '[' and ']'

     - "objects": sequences of string keys separated from JSON data by
       a colon, all separated by commas, and surrounded by braces '{'
       and '}'.

   Here is an example JSON value:

       { "f": 1,
         "g": [ 1, null, true, "burp", false, [ "a", "aa", "aaa" ] ],
         "name": [ 4, null, 5, null, { "a" : 10 } ]
       }

   JSON values can be arbitrarily nested as deep as you like. The
   official syntax description is in the IETF (Internet Engineering
   Task Force) RFC 7159:

     https://tools.ietf.org/html/rfc7159


   The first thing to do is to define a Haskell datatype to represent
   JSON values. The parser we will write will generate values in this
   datatype. Following the description given above, we use the
   following representation, with a constructor for each kind of thing
   that a JSON might be: -}

data JSON
  = Number  Int
  | Boolean Bool
  | String  String
  | Null
  | Array   [JSON]
  | Object  [(String,JSON)]
  deriving Show

{- For objects, we just store them as a list of key-value pairs. The
   JSON standard doesn't specify what happens if a key is repeated, or
   if JSON objects with the same key/values in a different order are
   to be treated as the same, so we will just store the keys in the
   order they are written.

   To write a parser for JSON values, we'll break it down into parsers
   for each of the individual kinds of JSON values.

   We already have parsers for 'Int's and string literals.

   Last lecture, we defined a parser for 'Bool's, but that was in
   Haskell syntax ("True" and "False"), not JavaScript syntax ("true"
   and "false"). So we define another parser for JSON booleans: -}

parseJSONBool :: Parser Bool
parseJSONBool =
  do parseLiteral "true"
     return True
  `orElse`
  do parseLiteral "false"
     return False

{- The other simple (in the sense of not being made of multiple bits)
   JSON value is "null". A parser for "null"s: -}

parseNull :: Parser ()
parseNull =
  do parseLiteral "null"
     return ()

{- The remaining kinds of JSON value to parse are arrays:

     [ 1, 2, null, false, "blarp" ]

   and objects:

     { "Aberdeen": 5, "Celtic": 0 }

   We could just launch in and write parsers for these two
   directly. But there is a bit of common structure that we can pull
   out. First, they both consist of sequences of things separated by
   commas. Second, they both allow whitespace to appear arbitrarily
   between elements.

   Let's handle the second one first. There are multiple whitespace
   characters in Unicode (spaces, tabs, newlines, non-breaking spaces,
   zero-width spaces, etc.), so we rely on the standard library
   function 'isSpace' to detect them for us: -}

parseSpace :: Parser ()
parseSpace =
  do c <- parseChar
     if isSpace c then return () else failParse

{- Arbitrary whitespace is zero or spaces: -}

parseWhitespace :: Parser ()
parseWhitespace =
  do zeroOrMore parseSpace
     return ()

{- Some tests:

      > runParser parseWhitespace "       stuff"
      Just ("stuff",())

      > runParser parseWhitespace "stuff"
      Just ("stuff",())

   Returning to the first point: items separated by commas. The
   'zeroOrMore' parser enabled us to parse zero or more repetitions. A
   common pattern is to have zero or more repetitions of items,
   separated by a character such as a comma or a semicolon. Can we use
   'zeroOrMore to build a parser for such repetitions?

   Here's one way of doing it. The function 'sepBy' takes two
   arguments: the first is a parser for the separator, and the second
   is a parser for the items. It returns a list of parsed items.

   It works by first trying to parse a single item (using 'p') and
   then zero or more repetitions of the 'separator' followed by a
   'p'. (The semicolon syntax allows us to write a two line "do" block
   on a single line.) If that parser fails, we fall back to just
   returning the empty list. -}

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator p =
  do x <- p
     xs <- zeroOrMore (do separator; p)
     return (x:xs)
  `orElse`
  return []

{- To test it, here's a parser for commas: -}

comma :: Parser ()
comma = parseLiteralChar ','

{- Let's try some examples, 'Int's separated by commas:

   1. Some items:

         > runParser (sepBy comma parseInt) "1,2,3,4"
         Just ("",[1,2,3,4])

   2. No items:

         > runParser (sepBy comma parseInt) ""
         Just ("",[])

   3. Some items with trailing stuff:

         > runParser (sepBy comma parseInt) "1,2,3,4xyz"
         Just ("xyz",[1,2,3,4])

   So far as expected, but JSON syntax also allows for spaces, which
   our parser doesn't:

         > runParser (sepBy comma parseInt) "1 , 2, 3, 4"
         Just (" , 2, 3, 4",[1])

   It recognised the '1', but the space between that and the comma
   caused it to stop consuming characters, and it returned the rest as
   left over input.

   How to fix this? We could modify 'sepBy' to allow extra whitespace,
   but this would prevent us from using it in situations where extra
   whitespace is disallowed. Another way to fix it is to alter the
   separator parser to allow whitespace before and after the comma: -}

spacedComma :: Parser ()
spacedComma =
  do parseWhitespace
     parseLiteralChar ','
     parseWhitespace

{- Using 'spacedComma' as a separator, we get the expected behaviour:

      > runParser (sepBy spacedComma parseInt) "1 , 2, 3, 4"
      Just ("",[1,2,3,4])

   JSON arrays are comma separated lists of values that are also
   surrounded by square brackets: '[' and ']'. Writing a parser for
   lists in this form is a matter of writing a small wrapper around
   'spacedComma' separated items. We leave the parser to be used for
   parsing the items as a parameter 'p' for now -- we will use the
   parser for JSON elements when we write it below. -}

parseList :: Parser a -> Parser [a]
parseList p =
  do parseLiteral "["
     parseWhitespace
     xs <- sepBy spacedComma p
     parseWhitespace
     parseLiteral "]"
     return xs

{- Some examples, for testing:

   1. Parsing a list of boolean:

        > runParser (parseList parseInt) "[ 1, 2, 3, 4 ]"
        Just ("",[1,2,3,4])

   2. Trailing commas are rejected:

        > runParser (parseList parseInt) "[ 1, 2, 3, 4,  ]"
        Nothing

   3. Empty lists:

        > runParser (parseList parseInt) "[ ]"
        Just ("",[])

   4. Lists without spaces:

        > runParser (parseList parseInt) "[1,2,3,4]"
        Just ("",[1,2,3,4])

   5. List with one item:

        > runParser (parseList parseInt) "[1]"
        Just ("",[1])

   6. Parsing a list of 'Int's, but giving it a boolean:

        > runParser (parseList parseInt) "[true]"
        Nothing

   7. Parsing lists of booleans:

        > runParser (parseList parseJSONBool) "[true]"
        Just ("",[True])
        > runParser (parseList parseJSONBool) "[true, false]"
        Just ("",[True,False])


   Now that we've done JSON arrays, we also want to parse JSON
   objects. Recall that a JSON object looks like:

         { "Aberdeen": 5, "Celtic": 0 }

   Like JSON arrays, they are comma separated lists of items,
   surrounded by brackets '{' and '}'. This time, each item consists
   of a pair of a string literal for the field name, a colon, and a
   value. Let's write a parser for object items that takes the parser
   to use for the value as an argument: -}

parseObjectItem :: Parser a -> Parser (String, a)
parseObjectItem p =
  do fieldname <- parseStringLiteral
     parseWhitespace
     parseLiteralChar ':'
     parseWhitespace
     value <- p
     return (fieldname, value)

{- Some tests:

   1. Successful object items:

         > runParser (parseObjectItem parseInt) "\"Aberdeen\" : 5"
         Just ("",("Aberdeen",5))
         > runParser (parseObjectItem parseInt) "\"Celtic\" : 0"
         Just ("",("Celtic",0))

   2. Fieldnames must have quotes:

         > runParser (parseObjectItem parseInt) "Celtic : 0"
         Nothing

   3. The colon is required:

         > runParser (parseObjectItem parseInt) "\"Celtic\" 0"
         Nothing

   4. But we don't need all the whitespace:

         > runParser (parseObjectItem parseInt) "\"Aberdeen\":5"
         Just ("",("Aberdeen",5))

   5. We can change what the parser used for the value is:

         > runParser (parseObjectItem parseJSONBool) "\"thisBooleanIs\":false"
         Just ("",("thisBooleanIs",False))

   Now to parse JSON objects, we write the same wrapper as for JSON
   arrays, except with a different parser for the items, and different
   kinds of brackets: -}

parseObject :: Parser a -> Parser [(String,a)]
parseObject p =
  do parseLiteral "{"
     parseWhitespace
     xs <- sepBy spacedComma (parseObjectItem p)
     parseWhitespace
     parseLiteral "}"
     return xs

{- A quick test:

     > runParser (parseObject parseInt) "{ \"Aberdeen\": 5, \"Celtic\" : 0}"
     Just ("",[("Aberdeen",5),("Celtic",0)])


   Now we are ready to write a parser for JSON values. Remember that a
   JSON value is either: a number (an 'Int' here), a string literal, a
   boolean, null, an array of JSON values, or a JSON object. We follow
   this specification directly, writing a parser that tries each in
   turn, falling through to the next case on failure using 'orElse': -}

parseJSON :: Parser JSON
parseJSON =
  do num <- parseInt
     return (Number num)
  `orElse`
  do s <- parseStringLiteral
     return (String s)
  `orElse`
  do b <- parseJSONBool
     return (Boolean b)
  `orElse`
  do parseNull
     return Null
  `orElse`
  do items <- parseList parseJSON
     return (Array items)
  `orElse`
  do fields <- parseObject parseJSON
     return (Object fields)

{- Some JSON test data, as a string (the backslashes at the end and
   start of each line are how we can write long strings in Haskell
   without having newlines in them): -}

testJSONString :: String
testJSONString =
  "{ \"f\": 1,\
   \  \"g\": [ 1, null, true, \"burp\", false, \
   \           [ \"a\", \"aa\", \"aaa\" ] ],\
   \  \"name\": [ 4, null, 5, null, { \"a\" : 1 } ]\
   \}"

{- Let's run it through the parser we just wrote (formatted for
   readbability):

   > runParser parseJSON testJSONString
   Just ("",Object [("f",Number 1),
                    ("g",Array [Number 1,
                                Null,
                                Boolean True,
                                String "burp",
                                Boolean False,
                                Array [String "a",
                                       String "aa",
                                       String "aaa"]]),
                    ("name",Array [Number 4,
                                   Null,
                                   Number 5,
                                   Null,
                                   Object [("a",Number 1)]])])

   Using the IO monad's 'readFile' function, we can now write a
   function that reads JSON data from a file. This parser is very
   strict: it does not allow any leading whitespace or anything after
   the JSON value. Can you change it so it is more liberal? -}

readJSON :: FilePath -> IO (Maybe JSON)
readJSON path =
  do fileContents <- readFile path
     case runParser parseJSON fileContents of
       Just ("", json) -> return (Just json)
       Nothing         -> return Nothing
       Just _          -> return Nothing

{-      Part III : Parsing arithmetic expressions

   Another example of a reasonably complex grammar is that of
   algebraic expressions like:

       (1 + 2) * 5 + 4

   which we want to parse into values of this datatype: -}

data Exp
  = Add Exp Exp
  | Mul Exp Exp
  | Num Int
  deriving Show

{- Often, the grammar for expressions like this is written like so:

     <exp> ::= <exp> + <exp>
             | <exp> * <exp>
             | <number>
             | ( <exp> )

   However, this grammar is ambiguous when used for parsing. If we
   have the input

      1 + 2 + 3

   Is it parsed as if it were

      [ 1 + 2 ] + 3

   or

      1 + [ 2 + 3 ]

   Another problem is mixing '+' and '*' without using parentheses to
   disambiguate.

   One way to avoid problems with the ambiguity in this grammar is to
   add a side note to the grammar to say that '+' and '*' both
   associate to the left (so we get the first parse) and that '+' has
   higher precedence that '*'. This is suitable for describing the
   grammar to humans, and to some parser generators. However, we can
   rewrite the grammar to one that removes the ambiguity, but still
   generates the same set of strings.

   We "stratify" the grammar into "expressions" that are sequences of
   factors that are added together, "factors" that are sequences of
   base items multiplied together, and "base items" that are either
   variable names, numbers, or expressions in parentheses. The grammar
   looks like:

     <exp>    ::= <factor> + <exp>
                | <factor>

     <factor> ::= <base> * <factor>
                | <base>

     <base>   ::= <varname>
                | <number>
                | ( <exp> )

   We can now almost directly translate this into our parsing
   langauge, if we put in a bit of extra bits to parse whitespace.

   First, <exp>ressions: -}

parseExp :: Parser Exp
parseExp =
  do e1 <- parseFactor
     parseWhitespace
     parseLiteralChar '+'
     parseWhitespace
     e2 <- parseExp
     return (Add e1 e2)
  `orElse`
  do e1 <- parseFactor
     return e1

{- Second, <factor>s: -}

parseFactor :: Parser Exp
parseFactor =
  do e1 <- parseBase
     parseWhitespace
     parseLiteralChar '*'
     parseWhitespace
     e2 <- parseFactor
     return (Mul e1 e2)
  `orElse`
  do e1 <- parseBase
     return e1

{- Third, <base> items: -}

parseBase :: Parser Exp
parseBase =
  do num <- parseInt
     return (Num num)
  `orElse`
  do parseLiteralChar '('
     parseWhitespace
     e <- parseExp
     parseWhitespace
     parseLiteralChar ')'
     return e

{- Some tests:

   > runParser parseExp "1"
   Just ("",Num 1)

   > runParser parseExp "1 + 2"
   Just ("",Add (Num 1) (Num 2))

   > runParser parseExp "1 + 2 * 3"
   Just ("",Add (Num 1) (Mul (Num 2) (Num 3)))

   > runParser parseExp "(1 + 2) * 3 + 4"
   Just ("",Add (Mul (Add (Num 1) (Num 2)) (Num 3)) (Num 4))

   > runParser parseExp "(1 + 2) * (3 + 4)"
   Just ("",Mul (Add (Num 1) (Num 2)) (Add (Num 3) (Num 4)))

   > runParser parseExp "((1) + 2) * (3 + 4)"
   Just ("",Mul (Add (Num 1) (Num 2)) (Add (Num 3) (Num 4)))


-}
