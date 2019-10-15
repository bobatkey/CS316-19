{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec07 where

import Prelude hiding (Left, Right, Maybe (..), Semigroup (..))
import Data.Char

{-     LECTURE 07 : MODELLING WITH DATATYPES

   Haskell takes types very seriously. Every program must have a type,
   and Haskell will refuse to run a program if it cannot check that a
   program is consistent in the way that it uses types. Sometimes this
   is annoying. There are programs that would work fine even though
   the type checker rejects them. For example,

      one :: Int
      one = if True then 1 else "one"

   Because 'if True' always evaluates to the 'then' part, the result
   of this program will always be an 'Int', so the type declaration
   'one :: Int' is correct. However, the Haskell type checker rejects
   this, because the 'else' part returns a string.

   But why would anyone write such a convoluted definition instead of
   writing 'one = 1'? In general, while types can be annoying, they
   can also be very useful in communicating to other programmers, and
   to the compiler, what we expect the shape of our data is. The basic
   philosophy of this course, and CS410 next year, is that, if you
   know something interesting about your program and its data, it is
   better to let the machine know too, so it can help you write and
   maintain the program.

   So what are types? Roughly speaking, types are sets of values, but
   Haskell has several ways of defining types which have different
   properties.

   In this lecture, I will go through the main ways of defining types
   in Haskell, first by giving new names to existing types, and then
   by defining new types. Finally, I will describe type classes -- a
   way of grouping types by what operations they allow. -}



{-      Part I : TYPE SYNONYMS

   It is sometimes useful to give new names to existing types. We
   might do this to for documentation purposes, or to avoid writing
   out a long type definition over and over again.

   We can give a new name to an existing type by making a declaration
   like so: -}

type Metres = Double

{- This defines 'Metres' as a synonym for the type 'Double', the
   built-in type of double precision floating point values. Now if we
   are writing some definitions where we intend the numbers to be
   interpreted as metres, then we can use 'Metres' as the type,
   instead of 'Double'. For example: -}

distanceToMoon :: Metres
distanceToMoon = 384402000
   -- https://en.wikipedia.org/wiki/Lunar_distance_(astronomy)

{- With the definition above, the name 'Metres' is just an alternative
   word for 'Double'. Wherever we can use a 'Double', we can also use
   'Metres'. For example, if we define a simple function on Doubles: -}

add :: Double -> Double -> Double
add x y = x + y

{- Then we can use it on 'Metres' with no complaints from the type
   checker, because it sees 'Metres' and 'Double' as the same type. -}

twiceDistanceToMoon :: Metres
twiceDistanceToMoon = add distanceToMoon distanceToMoon

{- Since, as far as the type checker is concerned, 'Metres' is exactly
   the same type as 'Double', using type synonyms like this is only
   for documentation purposes. There is nothing stopping us from
   making definitions that are nonsensical, like adding Metres and
   Seconds: -}

type Seconds = Double

secondsInAMinute :: Seconds
secondsInAMinute = 60

nonsense :: Double
nonsense = add secondsInAMinute distanceToMoon

{- To make Haskell distinguish between 'Metres' and 'Seconds', we will
   use a feature called 'newtype', described below. Nevertheless,
   giving new names, like 'Metres' and 'Seconds' to existing types can
   sometimes be a lightweight way of making the type signatures of
   functions easier to read.

   The second reason to give names to existing types is to give more
   meaningful names to complicated types, so that the type signatures
   of our functions are not littered with notation. For instance, if
   we wanted to use pairs of 'Int's as positions often in a program,
   we might make the following definition, defining the name
   'Position' to stand for the type '(Int,Int)': -}

type Position = (Int,Int)

{- Any pair of 'Int's can be used wherever a 'Position' is expected,
   making some definitions a bit more readable: -}

origin :: Position
origin = (0,0)

{- We can use type synonyms again when defining new type names. For
   instance, a "transformation" is a function that takes 'Position's
   to 'Position's. We can write this knowledge down as another type
   synonym declaration: -}

type Transformation = Position -> Position

{- Now when we write down some transformations, we can give a more
   meaningful type name than the original '(Int,Int) ->
   (Int,Int)'. For example: -}

goUp :: Transformation
goUp (x,y) = (x,y+1)

{- The type of 'goUp' is 'Transformation', which expands to 'Position ->
   Position', which expands to '(Int,Int) -> (Int,Int)'.

   Often, 'hiding' type definitions like this can improve code
   readability. It can also make code easier to modify. If we decided
   to change the representation of 'Position's to pairs of 'Double's,
   then we would just change the 'type Position = ...' definition and
   fix all the resulting type errors.

   However, it is not always the case that using type synonyms
   improves readability. Using the name 'Transformation' hides the
   fact that transformations are "really" functions, potentially
   obscuring the fact that they can be applied to 'Position's to
   transform them. Whether or not to use a type synonym depends on
   what you want to emphasise -- the underlying implementation, or the
   higher level idea. -}

{- Type synonyms can also take parameters, similar to how functions
   do. For example, -}

type Pair a = (a,a)

{- This defines a type /operator/ 'Pair', that when applied to another
   type 'X', stands for the type '(X,X)'. For example, we can define
   the type 'Position' again using 'Pair' like so: -}

type Position' = Pair Int

{- Now 'Pair Int' is equal to '(Int,Int)', which is equal to 'Position'
   as we defined above. -}



{-    Part II : DATA TYPE DEFINITIONS -}

{- Type synonyms give new names to existing types, but don't generate
   any new types or any new kinds of value. To generate new types, and
   the data values that they contain, we use 'data' declarations. We
   have already seen some examples of 'data' declarations in Lecture
   01 (the 'Direction' and 'List' types), Lecture 02 ('Maybe'), and
   Lecture 05 ('Tree'). Defining new datatypes using 'data' is the
   main way in Haskell of describing the kinds of data our programs
   manipulate.

   The simplest way to use 'data' is to define enumeration
   types. These are similar to "enum" types in Java:

      https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

   A standard example is days of the week: -}

data Weekday
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving Show

{- Here is the 'Direction' type from Lecture 01 again: -}

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving Show

{- In English, this says "define a datatype called 'Direction' whose
   values are either 'Up', 'Down', 'Left', or 'Right'. The 'deriving
   Show' part at the end tells Haskell to generate some code to print
   out these values. This will be covered in the section on Type
   Classes, below.

   As we have seen many times so far, we write functions that accept
   data as input using pattern matching. Here is a function that turns
   'Direction's into 'Transformation's (of 'Position's): -}

move :: Direction -> Transformation
move Up    (x,y) = (x,y+1)
move Down  (x,y) = (x,y-1)
move Left  (x,y) = (x-1,y)
move Right (x,y) = (x+1,y)

{- Unlike Java's enum types, datatypes in Haskell can also have
   additional data attached to them. We've seen several examples so
   far, including 'Cursor' and 'Process' in Exercise 1. For example,
   we could represent grid directions with attached distances like so: -}

data GridDirection
  = Vertical Int
  | Horizontal Int
  deriving Show

{- Meaning that we have two constructors of values in 'GridDirection',
   each of which takes an 'Int' as an additional argument. To show how
   these are used, we define functions by pattern matching that turn
   'Direction's into 'GridDirection's, and 'GridDirection's into
   'Transformation's: -}

toGridD :: Direction -> GridDirection
toGridD Up    = Vertical 1
toGridD Down  = Vertical (-1)
toGridD Left  = Horizontal (-1)
toGridD Right = Horizontal 1

moveOnGrid :: GridDirection -> Transformation
moveOnGrid (Vertical offset)   (x,y) = (x, y+offset)
moveOnGrid (Horizontal offset) (x,y) = (x+offset, y)

{- Just as for type synonyms, we can define datatypes with
   parameters. An exceedingly useful example of this is the 'Maybe'
   type, which is defined like so: -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- In English: "define a new datatype 'Maybe' for every type 'a', with
   two values: 'Nothing', which takes no parameters, and 'Just', which
   takes a parameter of type 'a'.

   'Maybe' is useful for situations where data may be missing. It is
   often used where 'null' would be a suitable value in other
   languages, except that the possibility of "no value" is recorded in
   the type. For example, if we write a function that searches for a
   particular key in a list of key-value pairs, we can use 'Nothing'
   to represent the case when the value is not found: -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []           = Nothing
search k ((k',v):kvs) = if k == k' then Just v else search k kvs

{- In words: searching for 'k' in the empty list returns 'Nothing';
   searching for 'k' in the list with (k',v) at the head returns 'Just
   v' if k is equal to k', and searches the rest of the list
   otherwise.

   While 'Maybe' is often used as a replacement for 'null', it is
   important to observe that it is marked explicitly in the type
   system. An analogue of this 'search' function in the Java standard
   library is the 'Map.get(Object)' method in the 'Map' interface:

      https://docs.oracle.com/javase/10/docs/api/java/util/Map.html#get(java.lang.Object)

   The documentation says that if the key searched for is not in the
   map, then this method returns null. However, it is valid for a key
   to be associated with the value 'null' in some implementations of
   'Map', so this is ambiguous.

   In Haskell, if the values could be 'null', then type of the list of
   key-value pairs would be of the form '[(k,Maybe v)]', and the
   'search' function would return values of type 'Maybe (Maybe
   v)'. Values of this type can be of one of three forms:

      1. Nothing       -- the key is missing
      2. Just Nothing  -- the key is present, but is mapped to Nothing
      3. Just (Just v) -- the key is present, and is mappped to 'v'

   In general, the presence of 'null' in programming language designs
   is now considered something of an antipattern. The inventor of
   'null', Sir Tony Hoare (also the inventor of QuickSort) now
   describes it as his "Billion dollar mistake", due to the number of
   bugs it has caused:

       https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare/

   Java now contains a class called 'Optional<V>' that is a clone of
   Haskell's 'Maybe' type. However, it is basically impossible to get
   rid of 'null' from Java completely.

   The lesson of 'Maybe' is that it is worth putting some effort into
   designing our types correctly to avoid illegal states from being
   representable, and to avoid confusion between different states. As
   an example, let's say that we want to represent student records
   with the following data:

     1. We always have student name

     2. We have either one or both of their registration number or DS
        username.

   If we try to do this in Java, we usually have to resort to comments
   to describe what is allowed:

        public class Student {
           // never null!
           @Nonnull
           private String name;

           // at least one of these is non-null
           private String registrationNumber;

           private String dsUsername;

           // ...
        }

   There is a '@Nonnull' annotation we can use to indicate that a
   particular reference field must be non null. However, the
   constraint that we have at least one of the registrationNumber and
   dsUsername is more difficult.

   In Haskell, we can make the following definitions: -}

data Student = MkStudent { name    :: String
                         , details :: StudentDetails
                         } deriving Show

data StudentDetails
  = RegNumber String
  | DSUsername String
  | RegNumAndDS String String
  deriving Show

{- This defines two datatypes. The first, 'Student', has a single
   constructor that takes two arguments. I have used another feature
   of Haskell, the ability to name the arguments to constructors. It
   is mainly here for documentation. Already in this type, we have
   encoded the fact that we *must* have a name for each student by
   *not* saying that we have a 'Maybe String'. We can also capture the
   constraint that we have at least one of the registration number or
   DS username by writing down the three cases explicitly in the
   'StudentDetails' type.

   The payoff for this is that it is not possible to make values of
   type 'Student' that do not satisfy the two constraints we listed
   above. By using proper type definitions, we have been able to take
   our comments and turn them into actionable advice for the machine
   to check our programs. -}


{- A special case of 'data' is when we have exactly one constructor
   which has exactly one argument. For example: -}

data Kilograms = MkKilograms Double
  deriving Show

{- This kind of declaration is very similar to the 'type Kilograms =
   ...' declaration from Part I, except that because 'data' always
   generates a *new* type, 'Kilograms' is different to 'Double'. This
   stops us from mixing up types as was permitted by just making type
   synonyms, at the cost of having to use pattern matching to
   decompose values.

   However, using 'data' for this purpose has a runtime cost. Due to
   the way that 'data' constructors in Haskell are considered to be
   lazy (we will cover this in Lecture 17), there is an overhead in
   the runtime representation of 'Kilograms'. To avoid this, Haskell
   offers another way of defining new types that are copies of
   existing types: 'newtype' -}

newtype Feet = MkFeet Double
  deriving Show

{- At runtime, the representation of 'Feet' is *exactly* the same as the
   representation of 'Double'. However, at compile time, the compiler
   treats these two types as distinct, and we have to use pattern
   matching and constructors to move between them.

   ASIDE: To see what the difference between 'data' and 'newtype' is
   in terms of laziness, have a look at this page from the Haskell
   wiki:

       https://wiki.haskell.org/Newtype

   or wait until Lecture 17. -}



{-    Part III : TYPE CLASSES -}

{- In most of the datatype declarations I have written so far, there
   have been mysterious "deriving Show" bits written underneath. On
   some of the function definitions, there have been obscure bits like
   'Eq a =>' and 'Ord a =>'. Both of these relate to a feature of
   Haskell called "type classes".

   Type classes are "classes" of types that all satisfy some common
   interface. For instance, the 'Eq' type class is the class of all
   types that have a function '==' that computes whether two values of
   that type are equal. Similarly, the 'Ord' type class is the class
   of all types that (a) are in the 'Eq' type classes, and (b) also
   have functions '<', '<=', '>=', '>', and 'compare' for making
   ordering comparisons. The 'Show' type class is the class of all
   types that have a 'show' function for converting them to strings.

   Type classes are similar to "interfaces" in Java, and have very
   little to do with "classes" in Java. Just as we might say that a
   class "MyClass" in Java might implement the "Comparable" interface,
   we might say that a type 'MyType' in Haskell implement (or "has an
   instance for") the 'Ord' type class.

   To write a function that uses functions from a type class for an
   unknown type, we must add a constraint to the type signature of the
   function to state that we require that this type is a member of
   that class. We saw an example with 'search' above:

      search :: Eq k => k -> [(k,v)] -> Maybe k

   This type says that we can use 'search' with any type of keys 'k',
   as long as that type is a member of the 'Eq' type class. As we will
   see below, 'Int' is a member of the 'Eq' type class, so we can use
   'Int's as keys:

       λ> search 4 [(1,"one"),(4,"four")]
       Just "four"

   However, it isn't possible in general to compare functions for
   equality, so we can't use functions as keys. Trying to do so yields
   an error:

       λ> search (\x -> x) [(\x -> x, "identity"), (not, "negation")]
       <interactive>:49:1-59: error:
        • No instance for (Eq (Bool -> Bool))
            arising from a use of ‘search’
            (maybe you haven't applied a function to enough arguments?)
        • In the expression:
            search (\ x -> x) [(\ x -> x, "identity"), (not, "negation")]
          In an equation for ‘it’:
              it = search (\ x -> x) [(\ x -> x, "identity"), (not, "negation")]

   The first bullet point tells us the problem: there is no instance
   (i.e., "implementation") of 'Eq' for the type 'Bool -> Bool'.

   It is also worth looking at what happens if we try to define
   'search' without including the 'Eq' constraint. Doing so yields the
   following error:

       Lec07.hs:253:28-34: error: …
           • No instance for (Eq k) arising from a use of ‘==’
             Possible fix:
               add (Eq k) to the context of
                 the type signature for:
                   search :: forall k v. k -> [(k, v)] -> Maybe v
           • In the expression: k == k'
             In the expression: if k == k' then Just v else search k kvs
             In an equation for ‘search’:
                 search k ((k', v) : kvs) = if k == k' then Just v else search k kvs

   Again, the relevant information is in the first bullet point. We've
   tried to use '==' on values of type 'k' without specifying that 'k'
   is a member of the 'Eq' type class.

   GHCi can give us information about currently defined type classes
   by using the ":info" feature:

       λ> :info Eq
       class Eq a where
         (==) :: a -> a -> Bool
         (/=) :: a -> a -> Bool
         {-# MINIMAL (==) | (/=) #-}
               -- Defined in ‘GHC.Classes’
       instance (Eq a, Eq b) => Eq (Either a b)
         -- Defined in ‘Data.Either’
       instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
       instance Eq Word -- Defined in ‘GHC.Classes’
       instance Eq Ordering -- Defined in ‘GHC.Classes’
       instance Eq Int -- Defined in ‘GHC.Classes’
       instance Eq Float -- Defined in ‘GHC.Classes’
       instance Eq Double -- Defined in ‘GHC.Classes’
         ....

   Asking for information on the 'Eq' type class first gives us its
   definition:

       class Eq a where
         (==) :: a -> a -> Bool
         (/=) :: a -> a -> Bool
         {-# MINIMAL (==) | (/=) #-}

   telling us that types in the 'Eq' type class actually have two
   'methods': (==) for equality, and (/=) for disequality. The comment
   'MINIMAL' tells us that we only need to define one of these, since
   the other can be defined in terms of it (by negation, in this
   case).

   The rest of the output tells us about all the types in the 'Eq'
   type class that GHCi currently knows about, including 'Int' as we
   used above. Defining new types with 'deriving Eq', defining our own
   instances of 'Eq', and importing other modules will extend this
   list.

   Haskell comes with definitions of 'Eq a' for many of the built in
   types. But what if we define our own types, like we did above? We
   have two options.

   First, we can get Haskell to define an equality test for us. This
   is what the 'deriving Eq' line does. For example: -}

data CompassDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

{- This 'data' declaration defines a four element type of compass
   directions, and asks the Haskell compiler to generate
   implementations of the 'Eq' and 'Show' type classes for us. This
   now allows us to compare 'CompassDirection's for equality:

      λ> North == North
      True
      λ> North == South
      False

   And to use them in functions that require a member of the 'Eq' type
   class, such as 'search' defined above:

      λ> search North [(North,Up),(South,Down)]
      Just Up
      λ> search West [(North,Up),(South,Down)]
      Nothing

   Deriving an implemenation of the 'Show' type class means that we
   get a function called 'show' that converts values of
   'CompassDirection' to 'String's. We can get information on 'Show'
   by using ':info' again:

      class Show a where
        showsPrec :: Int -> a -> ShowS
        show :: a -> String
        showList :: [a] -> ShowS
        {-# MINIMAL showsPrec | show #-}

   The useful function is called 'show'. The 'showsPrec' function is
   useful for outputting data without too many parentheses (using
   'prec'edence information), and 'showList' is sometimes a useful
   optimisation for outputting many values.

   Since we have derived a Show implementation for 'CompassDirection',
   we can use "show" on its values:

       λ> show North
       "North"
       λ> show South
       "South"

   The 'show' function is analogous to the 'toString' method on the
   'Object' class in Java.

   The ability of the Haskell compiler to generate instances of type
   classes for us is very useful, as long as we agree with the choices
   it makes. As an example of where we might want to differ from the
   default choice, let's look at the example of making a type of case
   insensitive strings. The only way that case insensitive strings
   differ from normal strings is that we regard two case insensitive
   strings as equal if they only differ in the cases of the characters
   in them. We make a 'newtype' of 'CaseInsenstiveString's, deriving
   'Show', but *not* 'Eq': -}

newtype CaseInsenstiveString = CIS String
  deriving Show -- but not 'Eq'!

{- We now use an 'instance' declaration to define what it means for two
   strings to be equal up to caseinsensitivity. We implement this by
   mapping all characters in both strings to upper case and then
   comparing them using normal string equality: -}

instance Eq CaseInsenstiveString where
  CIS str1 == CIS str2 =
    map toUpper str1 == map toUpper str2

{- In general, instance declarations define implementations for all the
   'methods' defined in the type class.

   Now we have:

       λ> CIS "case" == CIS "CASE"
       True
       λ> CIS "case" == CIS "CaSe"
       True
       λ> CIS "case" == CIS "C4S3"
       False

   Defining our own 'Eq' instances is often useful when we want to
   make more things equal than are usually so, as in this
   example. -}

{- Finally, it is possible to define our own type classes. It is not
   very common to do this in normal Haskell programming, because the
   standard library defines a large range of useful ones. As an
   example, I'll show two type classes from the standard library that
   will be very useful: semigroups and monoids.

   A semigroup is a type 'a' that has an operation:

     (<>) :: a -> a -> a

   which we read as some kind of "plus" or "combination" operation. We
   also expect this operation to be associative, meaning that it
   satisfies this equation:

     (x <> y) <> z == x <> (y <> z)

   A monoid is a semigroup that also has a 'zero' value, called
   'mempty', which satisfies the following laws:

     mempty <> x == x
     x <> mempty == x

   The simplest example of a monoid (or, more properly, a type with
   monoid structure), is the type 'Integer', with addition (+) as the
   (<>) and 0 as 'mempty'.

   Monoids are useful as an abstraction of what it means to have the
   ability to aggregate a number of data values into one. The (<>)
   operation combines two values, and the 'mempty' tells us what to do
   if we have no values. In the next two lectures, we'll see how to
   use Monoids for the purpose of data aggregation. For now, we'll see
   how to define the concepts of Semigroup and Monoid in Haskell, and
   how to define instances of those concepts.

   First we define the 'Semigroup' type class: (this type is already
   defined in the standard library, but I hid the definition in the
   import line at the top of this module). -}

class Semigroup a where
  (<>) :: a -> a -> a

{- This declaration says that there is a class of types called
   'Semigroup', and every member 'a' of this class has an operation
   called (<>) which has type 'a -> a -> a'.

   We can now define some instances of this class: -}

instance Semigroup Integer where
  (<>) = (+)

instance Semigroup Bool where
  (<>) = (&&)

{- Monoids are Semigroups with an extra operation, which we write like
   so: -}

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)

{- This says that a type 'a' is a Monoid if (1) it is already a
   'Semigroup', (2) it implements 'mempty', and (3) it implements
   'mappend' with the default implementation (<>).

   (The existence of the two names '<>' and 'mappend' for the same
   thing is a historical accident of the Haskell library, and nothing
   to do with the theory of semigroups or monoids.)

   Now we can extend our two Semigroup instances to be Monoids as
   well: -}

instance Monoid Integer where
  mempty = 0

instance Monoid Bool where
  mempty = True

{- Note that the equations for semigroups and monoids we stated above do
   actually hold for these definitions, but unfortunately Haskell has
   no way of checking this. The CS410 course next year will introduce
   the Agda system, which includes a proof language that allows for
   properties like this to be proved. The Liquid Haskell system is
   system which allows proving properties of Haskell programs by
   putting in special comments:

      https://ucsd-progsys.github.io/liquidhaskell-blog/

-}
