module Lec02 where

{-    LECTURE 02 : DEFINING FUNCTIONS

   So far, we have looked at:

     - Data and Functions (Lecture 01)

   In this lecture, we take a first look at defining our own
   functions.

   As the name implies, Functional Programming is all about defining
   our own functions. We will also start to see the interplay between
   defining functions and the data that those functions operate on. -}



{-    PART 1 : DECLARATIONS vs DEFINITIONS

   As we saw in Lecture 01, Haskell programs consist of two kinds of
   utterance:

     - _Declarations_, which introduce *new* types and values;

     - _Definitions_, which give a new way to compute with existing
       values.

   Here is an example of a declaration: -}

data MyType
  = Foo
  | Bar MyType
  deriving Show

{- As for the 'List' example in Lecture 01, we can read this declaration
   in English. We are declaring a new type 'MyType', and two new
   values of that type 'Foo' and 'Bar x', whenever 'x' is something of
   type 'MyType'. The 'deriving Show' is a special directive to tell
   the Haskell system to generate a function 'show :: MyType ->
   String' that allows us to print out values of 'MyType'.

   This was a 'declaration' because it introduced a new type and new
   values, distinct from any existing types or values.

   Here is an example of a definition: -}

myVal :: MyType
myVal = Foo

{- We have *defined* the name 'myVal' to stand for the existing value
   'Foo'. Remember that the first line states the type, while the
   second gives the actual definition.

   The right-hand sides of definitions (i.e., the text to the right of
   the '=') can be built up from several constructors: -}

myOtherVal :: MyType
myOtherVal = Bar (Bar Foo)

{- If we could only rename existing values, then definitions would not
   be too interesting. Definitions can also take parameters by naming
   them on the left-hand side of the 'equal'. When we have parameters,
   we mark this in the type by putting the types of the parameters to
   the left-hand side of an '->': -}

baz :: MyType -> MyType
baz x = Bar (Bar x)

{-     PART II : FUNCTIONS THAT MAKE DECISIONS

   Function definitions get even more interesting when they can change
   their behaviour based on their input. The first way we'll look at
   is using `if-then-else`, which is similar to the `if-then-else`
   constructs in other languages.

   The syntax of 'if-then-else' is:

       if <condition> then <then-case> else <else-case>

   where <condition> must be an expression of type 'Bool', and
   <then-case> and <else-case> are two expressions that must both be
   of the same type.

       ASIDE: If you are familiar with a language like Java, Haskell's
       if-then-else is a bit more general than the one in Java. In
       Java, 'if-then-else' can only be used on statements
       (i.e. commands that perform side effects), while Haskell's
       'if-then-else' can be used anywhere. The rough analogue to
       Haskell's 'if-then-else' in Java is the so-called ternary
       operator:

           <condition> ? <then-case> : <else-case>

   Let's use 'if-then-else' to define a 'not' function: -}

not0 :: Bool -> Bool
not0 x = if x then False else True

{- So, if the input to 'not0' is 'True', then the result is 'False'. If
   the input is 'False', then the result is 'True'.

   We can chain 'if-then-else's to make multiple decisions about the
   input. A good example is an implementation of Euclid's greatest
   common divisor (gcd) algorithm. Given two numbers 'x' and 'y', the
   greatest common divisor algorithm computes the largest number that
   divides into both 'x' and 'y' a whole number of times.

   Euclid's algorithm first checks to see whether 'x' is equal to 'y',
   if so, then it returns that number. Otherwise, it subtracts the
   smaller number from the larger, and computes the gcd of that and
   the smaller number.

   (See https://en.wikipedia.org/wiki/Greatest_common_divisor for more
   information.)

   We can write out this decision logic using Haskell's 'if-then-else'
   construct: -}

gcd0 :: Int -> Int -> Int
gcd0 x y = if x == y then x
           else if x < y then gcd0 x (y - x)
                else gcd0 (x - y) y

{- Defining functions by these chains of 'if-then-else's can get quite
   hard to read. Therefore, Haskell provides a special syntax called
   'guards' that allow us to arrange a chain of tests more
   readably. The syntax looks like:

      <function-name> <arguments> | <guard> = <expression>

   where <guard> is some expression of type 'Bool' that can mention
   the names in the arguments. They are called 'guards' because the
   expression on the right-hand side of the '='s is only used if the
   guard is true -- the guard guards the expression from being
   executed if its conditions are not met.

   When using guards it is important to note that Haskell stops at the
   first clause that matches when looking through all the lines of the
   definition. 

   Using guards allows us to write a neater looking version of the
   greatest common divisor function:  -}

gcd1 :: Int -> Int -> Int
gcd1 x y | x == y    = x
gcd1 x y | x < y     = gcd1 x (y - x)
gcd1 x y | otherwise = gcd1 (x - y) y

{- In the final case, we have used the predefined value 'otherwise' to
   indicate that we always execute the last case if we get
   there. Indeed, 'otherwise' is just defined to be 'True' in the
   standard library.

   We can also use guards to redefine the 'not' function from above: -}

not1 :: Bool -> Bool
not1 x | x         = False
       | otherwise = True

{- Here, we have used a shorthand notation that allows multiple guards
   with the same argument list -- we don't need to name the function
   and its arguments again in every line. -}


{-    PART III : DEFINING FUNCTIONS BY PATTERN MATCHING

   Guards and `if-then-else` allow us to define functions where we
   decide what the function does by using boolean logic to make the
   decision. This works well when we are defining functions that
   compare values for equality or ordering, or for functions that take
   booleans as inputs. However, Haskell allows us to declare datatypes
   that are much richer than plain numbers or booleans. 

   Of course, we can use pattern matching for booleans with no
   problem. Pattern matching allows a definition of the 'not'
   functions that is pretty much writing out what we want it to do: -}

not2 :: Bool -> Bool
not2 True  = False
not2 False = True

{- Sometimes, defining a function by pattern matching is as simple as
   the 'not2' function. Unfortunately, we also have to think a bit
   sometimes. Happily, we can use the type information to gently guide
   us in the right direction. We'll see this by considering the
   following problem: We want to define a function that takes a
   function and an input to that function, and returns the result of
   applying the function to that input. However, we also want to
   handle to case when the function, its argument, or both may be
   missing, and to act appropriately.

   The first thing to do is to make a first attempt at getting the
   right type for the function. The problem statement tells us that we
   have to deal with the case when the function or argument are
   missing, so we should represent this in the type. Forunately, the
   Haskell standard library comes with a type that allows us to
   represent missing information: the 'Maybe' type:

      > :info Maybe
      data Maybe a = Nothing | Just a
      [...]

   So 'Maybe a' has two values: 'Nothing', which represents missing
   information; and 'Just a', which represents present
   information. With this in mind, we can take a guess at the type of
   our function: the first argument is 'Maybe' a function, the second
   argument is 'Maybe' an 'a', and the result is 'Maybe' a 'b' (we
   might first guess that we should always try to produce a 'b', but
   the analysis below will show that this is impossible in general
   while also adhering to the specification given above). We leave a
   hole '_' where the definition will be:

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply x y = _

   To get started, we ask GHCi to tell us the context available for
   filling in the hole, and we learn that (we won't copy the whole of
   GHCi's output here -- it is too big!):

     (a) the goal is to produce something of type 'Maybe b'
     (b) we have 'x :: Maybe (a -> b)' and 'y :: Maybe a'

   To fulfil our obligation to produce a 'Maybe b', we refer to the
   output of ':info Maybe' above. We can either use 'Nothing', or we
   can use 'Just z', where 'z' must be of type 'b'. We could just
   complete the definition of 'maybeApply' by using 'Nothing', but
   this wouldn't satisfy the specification we are given -- if we do
   have a function and its argument, then we should use them.

   So, the first thing we do is to find out whether or not we have a
   function by pattern matching on the first argument:

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply (Just f) y = _
      maybeApply Nothing  y = _

   How we look at the first hole: we have to make something of type
   'Maybe b', and we have 'f :: a -> b' and 'y :: Maybe a'. So we have
   a way to make 'b's from 'a's (by applying 'f'), but we don't have
   an 'a'. The only source of 'a's to hand is the possible one in
   'y'. Therefore, we pattern match on 'y' to get two more cases:

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply (Just f) (Just x) = _
      maybeApply (Just f) Nothing  = _
      maybeApply Nothing  y        = _

   For the first hole, we now have 'f :: a -> b', 'x :: a', and we
   need to make a 'Maybe b'. We have apply 'f' to 'x' to get a 'b',
   and then use 'Just' to get a 'Maybe b'. So we fill in the first
   case:

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply (Just f) (Just x) = Just (f x)
      maybeApply (Just f) Nothing  = _
      maybeApply Nothing  y        = _

   In the second case, we have a function 'f :: a -> b' that can take
   us from 'a's to 'b's, but we have no 'a's!. In this case we will
   have no hope of making a 'b', so we return 'Nothing':

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply (Just f) (Just x) = Just (f x)
      maybeApply (Just f) Nothing  = Nothing
      maybeApply Nothing  y        = _

   In the third case, we have a choice: we could try to fill in the
   hole, or we could try to get more information by pattern matching
   on 'y'. A moment's thought tells us that pattern matching on 'y'
   will be fruitless if we want to produce a 'b': even if we get an
   'a', we won't be able to turn it into a 'b' because we don't have a
   function in the first argument position. So we replace the 'y' with a 

      maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeApply (Just f) (Just x) = Just (f x)
      maybeApply (Just f) Nothing  = Nothing
      maybeApply Nothing  y        = Nothing

   Finally, we can see that the last two lines have the same
   right-hand side, and do not use any variables from the left-hand
   side. Therefore, we can replace their patterns with "don't care"
   patterns, and collapse them into one line. This gives us our final
   definition: -}

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) (Just a) = Just (f a)
maybeApply _        _        = Nothing

{- For 'maybeApply', we could reason our way through the definition by
   thinking about the information available to us after each pattern
   match. Another useful technique for defining functions by pattern
   matching is to consider some representative examples and try to
   generalise from them.

   We saw the 'append' function in Lecture 01, but let's go through
   the definition slowly to see how it is made. We already know the
   type of the append function; it takes two lists of 'a's and
   produces a list of 'a's: -}

append :: [a] -> [a] -> [a]

{- Let's now think about what we want append to do on different
   inputs. It is always a good idea to think about the empty list
   first. What should
 
       append [] [4,5,6]

   be? [4,5,6] seems like a good choice. In general, appending 'ys' on
   to the empty list should just be 'ys', so let's write that down: -}

append [] ys = ys

{- What if the first list has something in it? Something like:

       append [1,2,3] [4,5,6]

   We want the final output to be [1,2,3,4,5,6], but just knowing the
   final result doesn't necessarily help us work out the steps
   required to get there. A helpful step is to look at what happens
   when we split the first list into its head and tail -- because this
   is exactly what pattern matching will give us:

       append (1 : [2,3]) ([4,5,6]

   Seeing that the final result has a '1' at the front, we can guess
   that we need to put a '1' at the front of the output:

       1 : ???

   The '???' now needs to be filled in with something that will
   generate [2,3,4,5,6]. But this is exactly what 'append [2,3]
   [4,5,6]' should do!:

       1 : append [2,3] [4,5,6]

   Continuing in this way, we can see that we can reconstruct our
   desired output step by step.

   To make our definition, we generalise from these particular lists,
   and replace '1' with 'x', '[2,3]' with 'xs', '[4,5,6]' with 'ys',
   to get the final line of our definition: -}

append (x:xs) ys = x : append xs ys

{- We can perform the same reasoning to construct a function that we
   haven't seen before: the function to reverse a list. Reversing a
   list takes a list as input and produces a list, so the type is: -}

rev :: [a] -> [a]

{- We look at the empty list case first. Reversing the empty list ought
   to just be the empty list: -}

rev [] = []

{- Reversing a list with something in it is a bit more complex. Let's
   look at two examples:

      rev [1,2,3]   = [3,2,1]
      rev [4,5,6,7] = [7,6,5,4]

   Bearing in mind the reasoning we used in the append definition,
   let's look carefully at what happens to the value at the head of
   the input lists. In both cases, it moved to the end. Similarly, the
   next element is moved to the position just before the end, and so
   on. We can decompose both examples into the following structure,
   using a very informal (non Haskell) notation:

      rev (x : xs) = "reversed xs" + x at the end

   Translating this definition into Haskell poses a problem: how do we
   put a value at the end of a list? The answer is to use the 'append'
   function to append it on the end: -}

rev (x:xs) = append (rev xs) [x]



{- Our next example is a function 'sawPrefix' that is designed to solve
   the following problem: given a list 'xs' and a list 'ys' remove
   'xs' from the front of 'ys' and return the rest of 'ys'.

   Here is an example:

      sawPrefix [1,2,3] [1,2,3,4,5,6] = [4,5,6]

   Hence, 'sawPrefix xs ys' "saws" the prefix 'xs' off the list 'ys'.

   Given the problem description, we can make a first guess at the
   type of 'sawPrefix'. To check that 'xs' is actually a prefix of
   'ys', we'll need to test elements for equality, so we'll need the
   'Eq' type class. Then we'll take two lists as input, and produce a
   list as output:

      sawPrefix :: Eq a => [a] -> [a] -> [a] 
      sawPrefix xs ys = _

   Now we can think about how to look at the input to decide what to
   do. One observation is that it is the first argument that is
   telling us what to do: it is the prefix that we are checking the
   second list against. So pattern matching on the first argument
   seems like a good idea:

      sawPrefix :: Eq a => [a] -> [a] -> [a] 
      sawPrefix []     ys = _
      sawPrefix (x:xs) ys = _

   In the first case, we have no more prefix to saw off. A list 'ys'
   with nothing sawed off is the list 'ys'. So we return 'ys' in this
   case:

      sawPrefix :: Eq a => [a] -> [a] -> [a] 
      sawPrefix []     ys = ys
      sawPrefix (x:xs) ys = _

   In the second case, we need to inspect the list 'ys' to check it
   against the prefix. So we pattern match:

      sawPrefix :: Eq a => [a] -> [a] -> [a] 
      sawPrefix []     ys     = ys
      sawPrefix (x:xs) []     = _
      sawPrefix (x:xs) (y:ys) = _

   Now we have a puzzle: we are checking that the second list has the
   first list as a prefix, but the first list has at least one element
   ('x'), and the second one has no elements! What should we return in
   this case? We seem to have two options:

      - We could try to fulfil our obligation to produce a list of
        'a's by cobbling together something from 'x', 'xs' and the
        empty list. However, there are no obviously good choices to
        make. We are meant to be checking that the first list is a
        prefix of the second, and we have discovered that this is not
        the case. To be good citizens, we should not try to cover up
        this problem, but try to report it back to the user of this
        function.

      - The second option is to alter the type of 'sawPrefix' so that
        it can report errors.

   Notice that the example of using 'sawPrefix' above only considered
   the happy case. Just attempting to write our function in Haskell
   has exposed the flaw in thinking only about the cases where
   everything goes smoothly.

   We select the second option, and signal erroneous input by using
   'Maybe' on the result type. This means that we can fill in the
   second case with 'Nothing'. We also need to update the first case
   by inserting a 'Just' to make it explicit that we not returning
   'Nothing' in that case:

      sawPrefix :: Eq a => [a] -> [a] -> Maybe [a] 
      sawPrefix []     ys     = Just ys
      sawPrefix (x:xs) []     = Nothing
      sawPrefix (x:xs) (y:ys) = _

   It now remains to do the final case: we have an expectation 'x' and
   a thing to check 'y'. If 'x' is equal to 'y', then all is good and
   we continue checking the rest of the prefix and the
   list. Otherwise, we return 'Nothing'.

   We program this logic using a guard: -}

sawPrefix :: Eq a => [a] -> [a] -> Maybe [a]
sawPrefix []     ys     = Just ys
sawPrefix (x:xs) []     = Nothing
sawPrefix (x:xs) (y:ys)
  | x == y    = sawPrefix xs ys
  | otherwise = Nothing

{- ASIDE: We can use the 'Maybe' type to make explicit the erroneous
   case in the 'hd' function we saw in Lecture 02: -}

hd :: [a] -> Maybe a
hd []     = Nothing
hd (x:xs) = Just x

{- END OF ASIDE -}

{-    PART IV : DEFINING FUNCTIONS USING OTHER FUNCTIONS

   We have seen how to define functions by using `if-then-else`s,
   guards, and pattern matching. Another way to define functions is
   solely in terms of previously defined functions. In this way, we
   can build up a toolbox of small functions that each do specific
   simple things into larger functions that perform complex
   operations.

   Our last example is the 'basename' function. We want this function
   to remove *suffixes* from lists, and in particular from strings
   (because in Haskell, strings are lists). For example, given
   "mylifestory.txt" and ".txt", 'basename' ought to return
   'mylifestory'. Since we were caught out above by not considering
   the error case, we also specify that 'basename' ought to return
   'Nothing' when the suffix is not present. So, for example, we
   should have:

      basename ".txt" "mylifestory.txt" == Just "mylifestory"
      basename ".txt" "mylifestory.pdf" == Nothing

   Let us make the following observations:

     - 'basename' is a lot like 'sawPrefix', but we want to get at the
       suffix, not the prefix.

     - we can turn the suffix of a list into its prefix by reversing
       it, and vice versa.

   This leads to a first guess at how to implement basename: -}

basename0 :: String -> String -> Maybe String
basename0 extension filename =
  sawPrefix (rev extension) (rev filename)

{- We have reversed the extension and the filename, turning suffixes
   into prefixes, and we have sawn off the extension if it
   exists. However, when we try to use this function, we run into
   trouble:

       > basename0 ".txt" "mylifestory.txt"
       Just "yrotsefilym"

   We need to reverse the output! But there is an obstacle. The output
   of 'sawPrefix' is of type 'Maybe String', and 'rev' expects an
   input of type 'String'. So we can't just apply 'rev' to the output
   of 'sawPrefix' -- 'rev' does not know what to do in the 'Nothing'
   case of a 'Maybe'.

   Fortunately, we have already considered the case of applying
   functions to possibly missing arguments above in the 'maybeApply'
   function. In this case, we always have a function to apply -- 'Just
   rev' -- so we can complete our definition: -}

basename :: String -> String -> Maybe String
basename extension filename =
  maybeApply (Just rev) (sawPrefix (rev extension) (rev filename))

{- Now we get:

       > basename ".txt" "mylifestory.txt"
       Just "mylifestory"

   as expected. -}
