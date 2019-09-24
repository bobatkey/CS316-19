{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Lec01 where

{-    LECTURE 01 : PROGRAMS MADE OF EQUATIONS; RUNNING THEM



   Welcome to the first lecture of the University of Strathclyde's
   "Functional Programming" course!

   Most of the lectures will be delivered by live Haskell coding on
   the projector. We take the final version of the code and annotate
   it with comments after the lecture to help you understand what is
   going on, and to recreate some of the things we said during the
   lecture.

   This first lecture introduces the two main concepts in Haskell:

     1. Defining what data is.

     2. Transforming data by pattern matching.

   We will cover both of these in a lot more depth later in the
   course. This lecture is a first introduction to the concepts. We
   will also introduce the concept of 'datatype' and how it relates to
   these concepts. -}



{-      PART 1 : DEFINING DATA AND VALUES

   In Haskell, pieces of data always start with capital letters, or
   are numbers, characters, or strings. Pieces of data that start with
   a capital letter are called 'constructors'.

     Examples: 'Nil', or 'True', or 'False', or '1234'.

   Before we can use any old capitalised word as a piece of data, we
   need to tell Haskell which words we want to use, and how they are
   grouped into datatypes. For example, 'True' and 'False' are grouped
   into the 'Bool' type.

   Let's introduce the concept of data and datatypes with an
   example. Here is a Haskell 'data' declaration that defines a new
   datatype 'Direction' with four constructors 'Up', 'Down', 'Left',
   'Right': -}

data Direction = Up | Down | Left | Right
  deriving Show

{- In English, we read this declaration as:

   A 'Direction' is either
   - 'Up'; or
   - 'Down'; or
   - 'Left'; or
   - 'Right'

   The symbol '|' is read as 'or'. So another way to read this
   declaration is: "a Direction is either Up, Down, Left, or Right".

     NOTE: the 'deriving Show' is an instruction to the Haskell
     compiler to generate a function for converting 'Direction's to
     strings. Think of this as auto-generating something similar to
     Java's 'toString()' methods.

   Now that have defined a datatype, we can make some value
   definitions. Here is a value definition: -}

whereIsTheCeiling :: Direction
whereIsTheCeiling = Up

{- This value definition consists of two lines:

   1. The first line tells us the name of the value we are defining
      ('whereIsTheCeiling') and what type it is ('Direction').

   2. The second line repeats the name and gives the actual
      definition. In this case, we are defining 'whereIsTheCeiling' to
      be 'Up'.

   We can now use the name 'whereIsTheCeiling' to stand for 'Up'.

   The first line is actually optional. The Haskell compiler is able
   to deduce the type from the value itself. So we can make another
   value definition 'whereIsTheFloor' by just giving the definition
   itself: -}

whereIsTheFloor = Down

{- It is good practice to always give the types though. It makes the
   code much more readable, and improves the error messages that you
   get back from the compiler.

   Making definitions like this is not very useful by itself. All we
   can do is give names to existing pieces of data. More usefully, we
   can define /functions/ that transform data. Here is an example that
   transforms directions by flipping them vertically: -}

flipVertically :: Direction -> Direction
flipVertically Up    = Down
flipVertically Down  = Up
flipVertically Left  = Left
flipVertically Right = Right

{- This definition looks more complex, but is similar to the previous
   ones. The first line tells us (and the Haskell compiler) what type
   'flipVertically' has. In this case it is a /function/ type
   'Direction -> Direction', meaning that it is a function that takes
   'Direction's as input, and returns 'Direction's as output.

   The other four lines define what happens for each of the four
   different 'Direction's: the two vertical directions are flipped,
   and the two horizontal directions remain the same. This style of
   writing functions is called "pattern matching" -- each line of the
   definition defines a pattern of input that it recognises, and
   defines what to do with it on the right hand side of the equals
   sign.

   Functions need not only convert data into data of the same
   type. For example, we can write a function by pattern matching that
   returns 'True' if the input is a vertical direction, and 'False' if
   is a horizontal direction: -}

isVertical :: Direction -> Bool
isVertical Up    = True
isVertical Down  = True
isVertical Left  = False
isVertical Right = False

{- We can also match on more than one input at a time. Here is a
   function that takes two 'Direction's as input and returns 'True' if
   they are the same, and 'False' otherwise. This definition also
   introduces a new kind of pattern: "wildcard" patterns, written
   using an underscore '_'. These patterns stand for any input that
   wasn't matched by the previous patterns. -}

equalDirection :: Direction -> Direction -> Bool
equalDirection Up    Up    = True
equalDirection Down  Down  = True
equalDirection Left  Left  = True
equalDirection Right Right = True
equalDirection _     _     = False

{- The type of 'equalDirection' is 'Direction -> Direction -> Bool',
   indicating that it takes two 'Direction's as input, and returns a
   'Bool'ean. The next four lines define the 'True' cases: when the
   two input 'Direction's are the same. The final line says that
   "whenever none of the previous cases applies, return 'False'". Note
   that patterns are tested in order, from top to bottom. -}

{-      PART 2 : RECURSIVE DATA

   Here is an example of a more complex Haskell 'data' declaration. it
   defines a new type 'List' that contains two constructors 'Nil' and
   'Cons': -}

data List a = Nil | Cons a (List a)
  deriving Show

{- In English, we read this declaration as:

   A 'List' of 'a's is either:
   - 'Nil'; or
   - 'Cons', followed by an 'a' and a 'List' of 'a's.

   The symbol '|' is read as 'or'. So another way to read this
   declaration is: "a List is either Nil or a Cons".

   Note that the type 'a' can stand for any other type. So we can have
   lists containing data of any type.

     NOTE: The names 'Nil' and 'Cons' to stand for the empty list and
     a list with one extra element originally come from the Lisp
     programming language, one of the oldest programming
     languages. 'Cons' is short for 'Construct'. For historical
     interest, see the original paper on Lisp by John McCarthy:

        John McCarthy: Recursive Functions of Symbolic Expressions and
        Their Computation by Machine, Part I. Commun. ACM 3(4):
        184-195 (1960)

        PDF at: http://www-formal.stanford.edu/jmc/recursive.pdf


   Now lets see some examples of data of type 'List a' for some 'a's,
   and how we can deduce that they are actually lists.

   Here is our first example: -}

ex1 :: List Int
ex1 = Cons 2 (Cons 7 (Cons 1 Nil))

{- Haskell syntax: as above, this definition consists of two lines:

   - The first line names the thing we are defining, 'ex1', and gives
     its type 'List Int' (meaning "List of Ints"). The symbol '::' is
     read as "has type".

   - The second line names the definition again, and the text to the
     right of the 'equals' is what we are defining 'ex1' to be.

   So, altogether, we are defining 'ex1', of type 'List Int', to be
   'Cons 2 (Cons 7 (Cons 1 Nil))'.

   In Haskell, we must always make sure that the definition we give
   matches the type. In the lecture, we worked through on the board
   how we can see that 'ex1' is of type 'List Int'. Let's go through
   it here:

   GOAL: 'Cons 2 (Cons 7 (Cons 1 Nil)) :: List Int'

   From what we said above, a list is either 'Nil' or a 'Cons'. In
   this case we have a 'Cons' and it must be followed by an 'Int'
   and a 'List Int'. So we check:

      * '2' is an 'Int' -- Yes!

      * 'Cons 7 (Cons 1 Nil)' is a 'List Int', this is a 'Cons', so we
        check again:

        - '7' is an 'Int' -- Yes!

        - 'Cons 1 Nil' is a 'List Int', so we check:

          + '1' is an 'Int' -- Yes!

          + 'Nil' is a 'List Int' -- Yes! because 'Nil' is one of the
            constructors of 'List a', for any 'a'.

   So we can now confidently say that 'Cons 2 (Cons 7 (Cons 1 Nil))'
   is of type 'List Int'. Of course, the Haskell compiler will check
   this for us, but it is useful to know what is going on when it does
   this check.

   If we load this file into the GHCi interactive Haskell REPL, we can
   see that it succeeds:

      GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
      Prelude> :l Lec01.hs
      [1 of 1] Compiling Lec01            ( Lec01.hs, interpreted )
      Ok, modules loaded: Lec01.
      *Lec01>

   Let's now see what happens when we try something that is not a
   valid list:

      ex2 :: List ??
      ex2 = Cons Nil (Cons 1 Nil)

   In this example (which is commented out so that you can load this
   file into GHCi for experimentation), we have not even been able to
   fill in the '??'. If we go through the same process as before for
   checking whether 'ex2' is a list we end up in a situation where
   'Nil' and '1' must have the same type. However, 'Nil' is a 'List a'
   (for some 'a'), and '1' is a number. If we try to get GHCi to
   accept our definition, it returns an error message:

      *Lec01> :l Lec01.hs
      [1 of 1] Compiling Lec01            ( Lec01.hs, interpreted )

      Lec01.hs:160:22:
          No instance for (Num (List a0)) arising from the literal ‘1’
          In the first argument of ‘Cons’, namely ‘1’
          In the second argument of ‘Cons’, namely ‘(Cons 1 Nil)’
          In the expression: Cons Nil (Cons 1 Nil)
      Failed, modules loaded: none.

   This error gives a lot of information, but in essence it is saying
   that Haskell cannot work out how to treat lists as numbers.

   Our final example shows that it is possible to have lists of lists,
   and that it is possible to get Haskell to infer types for us in
   many situations.

   Here, we just give a definition without a type. -}

ex3 = Cons Nil (Cons Nil Nil)

{- In general, this is bad style. It is very helpful for readers to be
   able to see the types of every definition in a Haskell program, as
   an aid to understanding. In this course, we will be careful to give
   types for all our definitions.

   However, it can be useful during development to leave off some of
   the types so that we can find out what Haskell thinks the types
   ought to be. We can do this by loading the file into GHCi and
   making a type query using the special command ':t' (short for
   ':type'):

      Prelude> :l Lec01.hs
      [1 of 1] Compiling Lec01            ( Lec01.hs, interpreted )
      Ok, modules loaded: Lec01.
      *Lec01> :t ex3
      ex3 :: List (List a)
      *Lec01>

   GHCi has told us that the type of 'ex3' is 'List (List a)' -- it is
   a list of lists of 'a's, for any type 'a'. -}

{- Haskell already has a built-in list type so we don't need to define
   our own every time we want to use lists. Lists are used so often in
   Haskell (possibly too much) that they get their own special syntax.

   Instead of writing the type 'List a', the built-in type is written
   '[a]', read as "list of 'a's".

   Instead of writing 'Nil', the empty list is written '[]'.

   Instead of writing 'Cons x xs', we write 'x : xs'. This is spoken
   as 'x cons xs'.

   It is possible to write lists using ':' and '[]', for example: -}

ex1' :: [Int]
ex1' = 2 : 7 : 1 : []

{- However, it is much more convenient to write them using the compact
   list notation: a sequence of values surrounded by square brackets,
   separated by commas. The three examples we gave above are written
   like so in this notation: -}

ex1'' :: [Int]
ex1'' = [2,7,1]

-- ex2'' :: [??]
-- ex2'' = [[], 1]

ex3'' :: [[a]]
ex3'' = [[], []]


{-    PART 2 : TRANSFORMING DATA BY PATTERN MATCHING

   As we say above, we define functions by /pattern matching/. This
   means that every function is a list of patterns of data that it can
   match with, and for each pattern a description of how that data is
   transformed. To define functions that operate on 

   Here is an example, which totals up all the elements in a list of
   'Int's: -}

total :: List Int -> Int
total Nil         = 0
total (Cons x xs) = x + total xs

{- As above, definitions consist of multiple lines. Here we have three
   lines: one type definition line, and two patterns. The first line
   describes the type of the thing we are defining. In this case, we
   are defining 'total' which has type 'List Int -> Int'. We read this
   as "functions that take Lists of Ints and return Ints". The
   analogous Java type would be:

      int total(List<Integer> input)

   Note that Haskell types go left to right!

   The second and third lines describe the two patterns of data that
   'total' matches on, and what it does in those two cases.

   'total Nil = 0' says "when total is applied to 'Nil', the answer is
   '0'". Put another way, the total of the empty list is '0'.

   'total (Cons x xs) = x + total xs' says "when total is applied to
   'Cons x xs', the answer is 'x' added to whatever the total of 'xs'
   is".

      NOTE: We have used a naming convention common in Haskell
      programming. The 'head' element of the list is called 'x'
      (because it is some unknown), and the rest of the list is called
      'xs' -- the "plural" of 'x'. In general, the '-s' suffix is used
      for lists.

      NOTE: Haskell programmers are sometimes criticised for their use
      of short names like 'x' and 'xs', rather than longer names like
      'theNumber' and 'restOfTheList'. Our feeling is that, while
      Haskell programmers do sometimes go overboard with short names,
      short names are very useful for maintaining a clear view of the
      *shape* of the code. When defining functions by pattern
      matching, it is often the shapes that are important, not the
      specifics.

   Running 'total' on some lists in GHCi should convince us that it is
   actually computing the totals of lists of Ints:

      *Lec01> total Nil
      0
      *Lec01> total (Cons 1 Nil)
      1
      *Lec01> total (Cons 1 (Cons 3 Nil))
      4
      *Lec01> total (Cons 1 (Cons 3 (Cons 5 Nil)))
      9

   We can also see how total works by stepping through the pattern
   matching process by hand. Let's take the third example:

      total (Cons 1 (Cons 3 Nil))
    =                               by the second rule for total
      1 + total (Cons 3 Nil)
    =                               by the second rule for total
      1 + (3 + total Nil)
    =                               by the first rule for total
      1 + (3 + 0)
    =                               by (built in) arithmetic
      1 + 3
    =                               by (built in) arithmetic
      4

   As you'll've seen in the "Evaluation Game" exercise, quite complex
   behaviour can be built up by pattern matching and reduction.

   Note that 'total' can only be applied to data that is of type 'List
   Int'. If we try to apply 'total' to a list of booleans ('True's and
   'False's), then GHCi will complain:

      *Lec01> total (Cons True Nil)

      <interactive>:14:13:
          Couldn't match expected type ‘Int’ with actual type ‘Bool’
          In the first argument of ‘Cons’, namely ‘True’
          In the first argument of ‘total’, namely ‘(Cons True Nil)’

   Importantly, GHCi complained *before* trying to execute the
   program. In Haskell, all type checking occurs before execution
   time. For this reason, Haskell is known as a "statically typed"
   language. This puts it in the same category as Java, though, as
   you'll see in this course, Haskell's type system is more expressive
   than Java's. An alternative (called "dynamic typing") is
   implemented in languages like Javascript, Python, Scheme, and other
   languages in the Lisp family.

   Let's see what would happen in a version of Haskell without a type
   checker:

      total (Cons True Nil)
    =                               by the second rule for total
      True + total Nil
    =                               by the first rule for total
      True + 0
    =                               cannot add booleans to Ints!
      << ERROR >>

   In Haskell, we try to avoid errors occurring at runtime like this
   by using types. Types are also useful as machine checked
   documentation for programs that describe the sorts of data that are
   expected as inputs and outputs for each part of the program. As we
   will see in this course, and much more so in the CS410 course in
   the fourth year, we can also use types to guide the process of
   writing programs. Our philosophy is that types are a design
   language that aids and guides us in writing programs. -}

{- 'total' is not the only function we can define on 'List's. Another
   useful function is 'append', which concatenates two lists. We again
   define this by pattern matching: -}

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

{- The type of 'append' states that it takes a 'List' of 'a's, another
   'List' of 'a's, and returns a 'List' of 'a's. Note that 'append' is
   polymorphic (or "generic") in the actual type of elements of the
   lists. We do not need to write a separate append function for lists
   of Ints and lists of Strings.

   Line two states that, when the first argument is 'Nil', the result
   is the second argument 'ys' (following the naming convention we
   described above). This makes sense: appending a list on to the
   empty list should just return the list.

   Line three states that, when the second argument is 'Cons x xs',
   the result is 'Cons x (append xs ys)'. That is -- we append xs to
   ys, and put the 'x' on the front. It might not be easy to see that
   this works straight away. Here is the example we used in the
   lecture:

     append (Cons "Unicorn" (Cons "Rainbow" Nil)) (Cons "Pegasus" Nil)
   =         { by the first rule }
     Cons "Unicorn" (append (Cons "Rainbow" Nil) (Cons "Pegasus" Nil))
   =         { by the first rule }
     Cons "Unicorn" (Cons "Rainbow" (append Nil (Cons "Pegasus" Nil)))
   =         { by the second rule }
     Cons "Unicorn" (Cons "Rainbow" (Cons "Pegasus" Nil))

   So, 'append' has successfully concatenated the two lists. You will
   have seen many more examples of 'append' in action in the
   Evaluation Game.

   We can also get GHCi to check our work:

       *Lec01> append (Cons "Unicorn" (Cons "Rainbow" Nil)) (Cons "Pegasus" Nil)
       Cons "Unicorn" (Cons "Rainbow" (Cons "Pegasus" Nil))

   Finally, we mention that, just as Haskell has a type of lists
   pre-defined, it also has functions for adding up lists and
   appending lists pre-defined. The function to add up a list is
   called 'sum' (indeed 'sum' is more general and can be used to add
   up any container containing numbers), and the function to append
   two lists is called '++' and is written in infix notation:

       *Lec01> ["Unicorn", "Rainbow"] ++ ["Pegasus"]
       ["Unicorn","Rainbow","Pegasus"]

   This concludes our first introduction to Haskell's data, functions,
   and types. -}
