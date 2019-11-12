{-# LANGUAGE InstanceSigs #-}
module Lec14 where

import Data.Char     (isDigit, digitToInt)

{-      Lecture 14 : PARSER COMBINATORS I

   This lecture and the next are about writing parsers.

   A parser is the part of a program that turns unstructured "flat"
   input into a structured representation suitable for processing by
   the rest of the program. For example, a program for computing
   arithmetic expressions such as a calculator takes in input like:

      "(1+1)*2"

   which is really a sequence of characters:

       '(', '1', '+', '1', ')', '*', '2'

   and it has a parser that turns it into a structured internal
   representation, such as:

       Mul (Add (Num 1) (Num 1)) (Num 2)

   that can be evaluated.

   Nearly every program has to do some form of parsing. Compilers and
   interpreters for programming languages are big examples with
   complex forms of input, but nearly every program must parse
   configuration files, input data (with formats like CSV, JSON, XML,
   etc.), network protocols, and so on.

   We saw a simple example of parsing in Exercise 1: 'splitOn' splits
   a "flat" string of fields separated by some character into the
   separate fields. We can use 'splitOn' to parse lines from CSV
   (Comma Separated Values) files:

       > splitOn ',' "CS316,Functional Programming,20"
       ["CS316", "Functional Programming", "20"]

   But 'splitOn' is a bit too simplistic. For example, how would we
   parse fields that contain commas? CSV files usually put fields that
   contain commas in quotes:

        "CS311,\"Programming Language, Design and Implementation\",20"

   which should be understood as:

        ["CS311", "Programming Language, Design and Implementation", "20"]

   But 'splitOn' will split this in the wrong way:

        ["CS311", "\"Programming Language", " Design and Implementation\"", "20"]

   Fixing this is an example of a parsing problem. In this lecture and
   the next, we'll see a general solution to this problem that will
   also work for many other parsing problems, like parsing the
   arithmetic expressions we saw above. -}


{-      Part I : Writing Parsers, a first attempt

   How should we write parsers in Haskell? One way to go about this is
   to have the following two thoughts:

   - Parsers take input that is a list of 'Char's, i.e., a 'String'.

   - Parsers can either fail to parse, because it recieved invalid
     input, or succeed, in which case it should return the structured
     representation.

   Thinking like this leads to the following definition ('version 1'): -}

type Parser_v1 a = String -> Maybe a

{- That is: a Parser of things of type 'a' is a function that takes
   'String's and either returns 'Nothing' if the input is malformed,
   or returns 'Just' of something if the input is wellformed.

   Let's try this idea with to parse 'Bool'ean values. We want to
   recognise the string "True" to return the value 'True', and the
   string "False" to return the value 'False'. The following function
   does this by using pattern matching: -}

parseBool_v1 :: Parser_v1 Bool
             -- String -> Maybe a
parseBool_v1 "True"  = Just True
parseBool_v1 "False" = Just False
parseBool_v1 _       = Nothing

{- This seems to work:

      > parseBool_v1 "True"
      True
      > parseBool_v2 "False"
      False

   But of course, we want to parse more complex input than just
   exactly "True" or "False". Could we use our parser to parse two
   booleans? We want to turn a string like:

      "TrueFalse"

   into the Haskell value:

      (True, False)

   But there seems to be no obvious way of doing this with our parsers
   as they are currently written. If we try to parse the string
   "TrueFalse" with 'parseBool_v1', it will fail because it is looking
   for exactly either the string "True" or the string "False".

   Wouldn't it be better to be able to reuse our parser for 'Bool's to
   parse a 'Bool', and then let something else parse the rest of the
   input?

   The reason we can't compose our parsers into larger parsers because
   the 'Parser_v1' type describes parsers that are "monolithic" --
   they make a decision about the whole input, and offer no way to
   pass what they don't understand on to another parser. We need
   modular parsers that can be chained together. The way we will do
   this is by making each parser responsible for parsing as much of
   the start of the input as it can, and then returning any left over
   input to be passed on to the next parser. -}


{-      Part II : Parsing with leftovers

   We upgrade our 'Parser_v1' to also return a 'String' on success
   like so: -}

newtype Parser a = MkParser (String -> Maybe (String, a))

{- Now a 'Parser' of things of type 'a' is a function that takes
   'String's as input and either returns 'Nothing', or 'Just
   (leftover, a)', where 'leftover' is the remainder of the input that
   wasn't parsed, and 'a' is the value understood from the beginning
   of the input. We'll give some examples below.

   We've also wrapped the type definition in a 'newtype' so that we
   can define some typeclass instances for it below (similarly to how
   we needed to use 'newtype' for the 'State' monad in Lecture 12).

   To run a parser, we need a function that unwraps the 'newtype' and
   runs the underlying parser on an input: -}

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) input = p input

{- Now we can write a parser that parses 'Bool's by looking at the start
   of the input. If it sees either "True" or "False", it returns the
   'rest' of the input, and the appropriate value. Otherwise, it
   returns 'Nothing'. -}

parseBool_v2 :: Parser Bool
parseBool_v2 =
  MkParser (\input ->
              case input of
                'T':'r':'u':'e':rest     -> Just (rest, True)
                'F':'a':'l':'s':'e':rest -> Just (rest, False)
                _                        -> Nothing)

{- Let's try it:

      > runParser parseBool_v2 "True"
      Just ("", True)
      > runParser parseBool_v2 "False"
      Just ("", False)
      > runParser parseBool_v2 "True101010"
      Just ("101010", True)
      > runParser parseBool_v2 "Truthy"
      Nothing
      > runParser parseBool_v2 "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   Notice that, in the case of a successful parse, the remainder of
   the input is returned along with the result of the parsing. We can
   use this to chain two parsers together, passing the leftover input
   from the first into the second.

   Here is a function that takes two 'Parser's, one for 'a's and one
   for 'b's, and creates a 'Parser' for pairs '(a,b)' using the
   following strategy:

     1. It takes the input in the variable 'input'.

     2. It runs the first parser on 'input'. If it fails (returns
        'Nothing') the combined parser returns 'Nothing'.

     3. If the first parser succeeds with leftovers 'input2' and
        result 'a', then it runs the second parser on 'input2'. If
        that fais, the combined parser returns 'Nothing'.

     4. If the second parser succeds with leftovers 'input3' and
     result 'b' then the combined parser returns leftovers 'input3'
     and the final result '(a,b)'.

   Here is the Haskell code for this strategy: -}

parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair p1 p2 =
  MkParser (\input -> case runParser p1 input of
                        Nothing ->
                          Nothing
                        Just (input2, a) ->
                          case runParser p2 input2 of
                            Nothing ->
                              Nothing
                            Just (input3, b) ->
                              Just (input3, (a,b)))

{- We can use 'parsePair' to parse two 'Bools', one after the other by
   calling 'parsePair' with 'parseBool_v2' for both arguments: -}

parsePairOfBools :: Parser (Bool, Bool)
parsePairOfBools = parsePair parseBool_v2 parseBool_v2

{- Some example runs:

      > runParser parsePairOfBools "TrueTrue"
      Just ("", (True, True))
      > runParser parsePairOfBools "FalseTrue"
      Just ("", (False, True))
      > runParser parsePairOfBools "FalseTrueLEFTOVERS"
      Just ("LEFTOVERS", (False, True))
      > runParser parsePairOfBools "FalsTru"
      Nothing

   Let's look at 'parsePair' in more detail.

   The cascade of 'case's with 'Nothing' always resulting in 'Nothing'
   is similar to way that we modelled exceptions using 'Maybe' in
   Lecture 10. Also, the passing and returning of the left over input
   from one parser to the next ('input', 'input2', and 'input3') looks
   very similar to how the state value was passed through when we
   modelled mutable state in Lecture 11.

   Both 'Maybe' and 'State' were instances of 'Monad', which allowed
   us to use generic tools for all 'Monad's, as we saw in Lecture 12.
   Could it be that 'Parser' is a 'Monad' too? Let's try to implement
   the 'Monad' interface: -}

instance Monad Parser where
  return x = MkParser (\input -> Just (input, x))
{- The 'return' for 'Parser's is very similar to the combination of
   'return' for 'Maybe', which always succeeded by returning 'Just x',
   and 'return' for 'State', which returned the state value (here
   'input') without modification. In terms of parsing, 'return x' is a
   parser which consumes no input and always succeeds. -}

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= k =
    MkParser (\input ->
                case runParser p input of
                  Nothing ->
                    Nothing
                  Just (input2, a) ->
                    runParser (k a) input2)
{- '>>=' ("bind") for 'Parser's is also like a combination of the '>>='s
   for 'Maybe' and 'State'. It takes the initial input 'input' and
   passes it to the first 'Parser' 'p'. If that fails, then (like
   'Maybe') it returns 'Nothing'. If it succeeds with leftovers
   'input2' and result 'a', it calls 'k' with 'a' to get the
   continuation 'Parser' and runs it with 'input2'. -}

{- Now that we have defined 'Parser' to be an instance of 'Monad', we
   can use "do" notation with 'Parser's. Here is how we can parse
   pairs of 'Bool's using the 'parseBool' parser we wrote above: -}

parsePairOfBools_v2 :: Parser (Bool, Bool)
parsePairOfBools_v2 =
  do b1 <- parseBool_v2
     b2 <- parseBool_v2
     return (b1, b2)

{- In words: first we parse a boolean, call it 'b1', then we parse a
   boolean, call it 'b2'. Finally, we return the pair '(b1,b2)'. -}



{-       Part III : Building Parsers

   The definitions of the 'Monad' functions for 'Parser' form the
   foundation of how we're going to build complicated parsers by
   putting together simple parsers. The bind ('>>=') from the 'Monad'
   interface lets us put one parser after another to parse two things
   in sequence.

   The simplest parser we'll use is one that just parses a single
   character from the input. If there is a character in the input, it
   consumes it and returns it. Otherwise it fails. Here is the
   definition: -}

parseChar :: Parser Char
parseChar =
  MkParser (\input -> case input of
                        c:input1 -> Just (input1, c)
                        _        -> Nothing)

{- Some examples:

      > runParser parseChar "hello"
      Just ("ello", 'h')
      > runParser parseChar "ello"
      Just ("llo", 'e')
      > runParser parseChar ""
      Nothing

   From these examples, we can see that 'parseChar' only fails if the
   input is empty. Otherwise, it returns the first character in the
   input.

   It'll also be useful to have a 'Parser' that always fails (similar to
   'failure' we defined for 'Maybe' in Lecture 10). We'll use this
   whenever we read a piece of input that we don't want. -}

failParse :: Parser a
failParse =
  MkParser (\input -> Nothing)

{- This parser always fails, no matter what input we give it:

      > runParser failParse ""
      Nothing
      > runParser failParse "hello"
      Nothing

   We can put the 'Parser's 'parseChar' and 'failParse' together to
   write a parser that only succeeds on the character 'expected' we
   give it. It first uses 'parseChar' to read a single character
   'got'. If 'got' and 'expected' are the same thing, then it returns
   '()', signaling success. If they are not equal, it uses 'failParse'
   to reject this input. -}

parseLiteralChar :: Char -> Parser ()
parseLiteralChar expected =
  do got <- parseChar
     if got == expected then return () else failParse

{- Some examples:

      > runParser (parseLiteralChar 'h') "hello"
      Just ("ello", ())
      > runParser (parseLiteralChar 'h') "ello"
      Nothing
      > runParser (parseLiteralChar 'h') ""
      Nothing

   If we can write a parser that checks for specific characters, we
   can use it with a list of specific characters to check for specific
   strings of characters. We already have a function that performs
   some action for every element of a list: 'mapM_' from Lecture
   12. Using this, we get a parser that takes a specific 'String', and
   suceeds only if that string is at the start of the input: -}

parseLiteral :: String -> Parser ()
parseLiteral = mapM_ parseLiteralChar

{- For example:

      > runParser (parseLiteral "hello") "hello"
      Just ("", ())
      > runParser (parseLiteral "hullo") "hello"
      Nothing
      > runParser (parseLiteral "hello") "hello world"
      Just (" world", ())


   EXERCISE: Rewrite 'parseLiteral' using 'for_'.


   Using 'parseLiteralChar', we can write a parser for 'Bool's without
   having to write the underlying parser directly, but reading the
   first character and then deciding whether to try to read a "True"
   or a "False": -}

parseBool_v3 :: Parser Bool
parseBool_v3 =
  do c <- parseChar
     case c of
       'T' -> do parseLiteral "rue"
                 return True
       'F' -> do parseLiteral "alse"
                 return False
       _   -> failParse

{- This has the same behaviour as the 'parseBool_v2' we wrote above:

      > runParser parseBool_v3 "True"
      Just ("", True)
      > runParser parseBool_v3 "False"
      Just ("", False)
      > runParser parseBool_v3 "True101010"
      Just ("101010", True)
      > runParser parseBool_v3 "Truthy"
      Nothing
      > runParser parseBool_v3 "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   This way of writing a parser for 'Bool'eans works, but it is a bit
   clumsy. What if we have a lot of words to recognise? What if a lot
   of them share the first few letters? Definitions like this would
   get quite messy.

   If we think back to Lecture 10 on using 'Maybe' to model
   exceptions, we had the function 'catch' for modelling the idea of
   "trying one thing, and if that fails, trying another". We could use
   something like this for writing 'Parser's: we try one parser (for
   parsing the word "True", say), and if that doesn't work try another
   (e.g., for parsing the word "False").

   We'll call the function that tries one parser and then another
   'orElse'. Its definition is very similar to the one for 'catch',
   except that we also pass the current 'input' into each parser: -}

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Nothing -> runParser p2 input
                Just (rest,x) -> Just (rest,x))

{- Some examples, showing how it is used in an infix position to parse
   either an 'a' or a 'b' at the start of the input:

      > runParser (parseLiteralChar 'a' `orElse` parseLiteralChar 'b') "abc"
      Just ("bc", ())
      > runParser (parseLiteralChar 'a' `orElse` parseLiteralChar 'b') "bca"
      Just ("ca", ())
      > runParser (parseLiteralChar 'a' `orElse` parseLiteralChar 'b') "cab"
      Nothing

   We can also see the fail-and-try-the-other-one behaviour directly
   if we try 'failParse' or else something:

      > runParser (failParse `orElse` parseLiteralChar 'a') "abc"
      Just ("bc", ())


   EXERCISE: For the 'Monad' instance for 'Parser', I said that it is
     a combination of 'Maybe' (for simulating exceptions), and 'State'
     (for simulating mutable state). So you could also do parsers in a
     language like Java that has built-in state and exceptions. (Try
     it!  use an instance field to keep the current input in.) What is
     different about the 'orElse' function? What facility would you
     need in Java to make it work?

   With the ability to try one parser and fall back to another if it
   fails, we can write parsers that are built from several
   possibilities, and return different results from according to what
   happened. For example, here is a parser that first tries to match
   the input with "True", if that works then it returns 'True';
   otherwise it tries to match the input with "False" and if that
   works it returns 'False': -}

parseBool :: Parser Bool
parseBool =
  do parseLiteral "True"
     return True
  `orElse`
  do parseLiteral "False"
     return False

{- This has the same behaviour as 'parseBool_v2' and 'parseBool_v3' we
   wrote above:

      > runParser parseBool "True"
      Just ("", True)
      > runParser parseBool "False"
      Just ("", False)
      > runParser parseBool "True101010"
      Just ("101010", True)
      > runParser parseBool "Truthy"
      Nothing
      > runParser parseBool "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   An important point about 'orElse' is that it represents *ordered*
   choice. If the first parser succeeds, then its result is used, even
   if the second one could have succeeded. For example, the following
   parser built from 'orElse' will greedily match "Four", and never
   let the other parser have a go: -}

orderedDemo :: Parser String
orderedDemo =
  do parseLiteral "for"
     return "the keyword 'for'"
  `orElse`
  do parseLiteral "forty"
     return "the number 40"

{- We can see this behaviour in some examples:

     > runParser orderedDemo "for"
     Just ("","the keyword 'for'")
     > runParser orderedDemo "forty"
     Just ("ty","the keyword 'for'")

   On the input "forty", the first parser for "for" matches, and then
   we get the result "the keyword 'for'" and leftover input "ty". The
   second parser never gets to run.

   This ordered behaviour is often useful, because it gives us unique
   (unambiguous) answers if there are multiple ways of parsing the
   same input. However, it does mean that the 'Parser's we write might
   not parse the input in the way that we intend. It is always a good
   idea to have a collection of test inputs for testing your
   'Parser's. -}

{- We now have the following basic 'Parser's and ways of combining
   them:

     1. The 'parseChar' 'Parser' for parsing single characters.

     2. The monad interface 'return' and '>>=' for parsers that do
        nothing and sequence two parsers.

     3. The 'failParse' parser that always fails.

     4. The 'orElse' parser that tries one parser, and if that fails
        tries another.

   With these basic parsers, we have built 'parseLiteral' that
   recognises literal strings. And we can now go on to build more
   useful parsers on top of these.

   With these basic functions, we no longer have to write anything
   involving 'MkParser' directly. All the other parsers we will write
   will be written in terms of the basic functions.

   The first useful reusable parser we'll write is one that parses
   lists of items by repeatedly trying to run a given 'Parser' until
   it fails. We call this parser 'zeroOrMore' because it attempts to
   read zero or more parses of the parser it is given from the input: -}

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
   return []

{- Some examples:

     > runParser (zeroOrMore (parseLiteralChar 'a')) "aaaa"
     Just ("",[(),(),(),()])
     > runParser (zeroOrMore (parseLiteralChar 'a')) "aaaabb"
     Just ("bb",[(),(),(),()])
     > runParser (zeroOrMore parseBool) "aaaabb"
     Just ("aaaabb",[])
     > runParser (zeroOrMore parseBool) "TrueFalseTrue"
     Just ("",[True,False,True])
     > runParser (zeroOrMore parseBool) "TrueFalseTrueaaaa"
     Just ("aaaa",[True,False,True])

   Note that 'zeroOrMore p' never fails: if it can't parse the input
   with 'p', then it just returns the empty list (see the third
   example above).

   'zeroOrMore p' is also greedy: it parses as far into the input as
   it can, no matter what later parses in a sequence are
   expecting. For example, the following parser uses 'zeroOrMore'
   twice to parse two lists of 'Bool'eans: -}

twoBoolLists :: Parser ([Bool],[Bool])
twoBoolLists =
  do bools1 <- zeroOrMore parseBool
     bools2 <- zeroOrMore parseBool
     return (bools1, bools2)

{- If we try this on any input, the second list will always be empty,
   because the first occurrence of 'zeroOrMore parseBool' will have
   greedily consumed all the input the second 'zeroOrMore parseBool'
   could have read:

      > runParser twoBoolLists "TrueFalseTrue"
      Just ("",([True,False,True],[]))
      > runParser twoBoolLists "TrueFalseTrueaaaa"
      Just ("aaaa",([True,False,True],[]))
      > runParser twoBoolLists "aaa"
      Just ("aaa",([],[]))
-}

{- Example: Parsing numbers

   We'll end this lecture with a practical example of using
   'Parser's. Parsing numbers from a decimal representation in a text
   file to actual 'Int's is a common operation for reading many kinds
   of configuration files or programming languages.

   Let's start by defining a 'Parser' for reading individual
   digits. It is useful to know that there are two helpful functions
   in the standard library's 'Data.Char' module:

     - 'isDigit :: Char -> Bool' that returns 'True' if it is given a
       digit character, and 'False' otherwise.

     - 'digitToInt :: Char -> Int' that returns the numerical
       value of a digit character.

   Putting these together with 'parseChar' and 'failParse', we get the
   following parser that recognises individual digits and converts
   them to integers in the range 0..9: -}

parseDigit :: Parser Int
parseDigit = do c <- parseChar
                if isDigit c then
                  return (digitToInt c)
                else
                  failParse

{- Some examples:

       > runParser parseDigit "1"
       Just ("",1)
       > runParser parseDigit "2"
       Just ("",2)
       > runParser parseDigit "x"
       Nothing
       > runParser parseDigit "A"
       Nothing
       > runParser parseDigit "123"
       Just ("23",1)
       > runParser parseDigit "3aa"
       Just ("aa",3)

  EXERCISE: Try writing 'parseDigit' without using 'isDigit' and
    'digitToInt', and instead using a 'case' to pattern match on the
    character 'c' to determine which digit (or not) it is.


  We can now use 'parseDigit' to parse positive whole numbers. Numbers
  will always have at least one digit, so we parse a single digit
  first, using 'parseDigit'. Then we use 'zeroOrMore parseDigit' to
  parse as many more digits as we can. Finally, we use a 'foldl' to
  convert the leading digit and the list of digits into an actual
  number (see the tutorial on Recursion Schemes for this use of
  foldl): -}

parsePositiveNumber :: Parser Int
parsePositiveNumber =
  do digit  <- parseDigit
     digits <- zeroOrMore parseDigit
     return (foldl (\n d -> n*10 + d) digit digits)

{- Some examples to test it:

     > runParser parsePositiveNumber "1"
     Just ("",1)
     > runParser parsePositiveNumber ""
     Nothing
     > runParser parsePositiveNumber "123"
     Just ("",123)
     > runParser parsePositiveNumber "123aaa"
     Just ("aaa",123)

  Now to complete a parser for 'Int's, we also need to deal with the
  possibility of negative numbers. Negative numbers are usually
  indicated by having a '-' sign before them. To be modular, we write
  a little parser that returns '-1' if it sees a '-' sign, and '1' if
  it doesn't: -}

parseSign :: Parser Int
parseSign = do parseLiteralChar '-'
               return (-1)
            `orElse`
            do return 1

{- Now we put 'parseSign' and 'parsePositiveNumber' together to parse
   'Int's: -}

parseInt :: Parser Int
parseInt = do sign <- parseSign
              num  <- parsePositiveNumber
              return (sign * num)

{- Some tests:

       > runParser parseInt "100"
       Just ("",100)
       > runParser parseInt "1000"
       Just ("",1000)
       > runParser parseInt "1001"
       Just ("",1001)
       > runParser parseInt "1001aaa"
       Just ("aaa",1001)
       > runParser parseInt "-50"
       Just ("",-50)
       > runParser parseInt "-30"
       Just ("",-30)
       > runParser parseInt "- 30"
       Nothing
       > runParser parseInt "--30"
       Nothing
-}

{- EXERCISE: Write a 'Parser' for string literals. A string literal is
     how you write strings in a language like Java or Haskell. It is a
     sequence of characters in quotes, for example:

         "hello, I'm a string literal"

     String literals always end at the first '"'. To write string
     literals that represent strings containing '"'s you need to
     "escape" them by writing a '\' in front of them:

         "this is a \"string\" literal"

     Hint: you'll need to do the following:

       1. Define a parser for "string literal characters" that allows
          any character *except* '"', unless it is preceeded by a '\'.

       2. Write a parser for string literals that expects a '"', zero
       or more string literal characters, and a following '"'.

     Note that when testing your parser, you'll need to escape the
     '"'s and '\'s in your input to be able to write your string
     literals as Haskell string literals. That is, the following test
     cases ought to work:

         > runParser parseStringLiteral "\"hello, I'm a string literal\""
         Just ("", "hello, I'm a string literal")
         > runParser parseStringLiteral "\"this is a \\\"string\\\" literal\""
         Just ("", "this is a \"string\" literal")
-}



{----------------------------------------------------------------------}
{- Appendix                                                           -}

{- The following two type class instance definitions are required by the
   way that the Monad type class is defined in standard Haskell. We'll
   come back to why in Lecture 16. -}

instance Applicative Parser where
  pure      = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor Parser where
  fmap f p = pure f <*> p
