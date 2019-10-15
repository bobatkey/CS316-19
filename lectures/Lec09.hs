module Lec09 where

import Prelude hiding ( Monoid (..)
                      , Foldable (..)
                      , Functor(..)
                      , all
                      , Maybe (..) )

{-        LECTURE 09 : FUNCTORS AND CONTAINERS

   So far, we have looked at several kinds of data structure that
   'contain' some other kind of data. Our most common example has been
   the 'lists'. We've seen the definition of list multiple times so
   far, but here it is again using the standard Haskell notation
   rather than the 'Nil' and 'Cons' constructors.

      data [a] = [] | a :: [a]

   Lists store data in a sequence, one after the other.

   Another example of a container-like structure is a tree: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- 'Tree's store data organised into a tree-like shape, with a single
    root and where each piece of data has two 'children'. Trees like
    this are often useful for storing data for quick lookup, when used
    as binary search trees. In Lecture 06 (Declaring Types and
    Classes), we saw some other kinds of tree suitable for storing
    other kinds of data.

    Another kind of container we've seen in this course is 'Maybe'. It
    may not seem sensible at first sight to see of this as a
    container, but we can think of a value of type 'Maybe a' is either
    containing zero or one 'a's. This is similar to how list of 'a's
    can contain zero, one, two, ... 'a's. -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- We now have three different kinds of container types: lists, trees,
   and 'Maybe's. This lecture is about what they have in common that
   makes them 'container'-like, and how we express that in Haskell. -}

{-    PART I : FUNCTORS -}

{- In Lecture 05 (Higher Order Functions), we saw the function 'map',
   which applies a function to every element of a list to yield a new
   list of tranformed elements:

      map :: (a -> b) -> [a] -> [b]

   In Exercise 3.1.2, you are asked to define a map operation for
   'Tree's, which has this type:

      mapTree :: (a -> b) -> Tree a -> Tree b

   These functions both a do similar thing: they take a function 'f',
   some structure containing values of type 'a', and return the *same*
   structure, but this time containing values of type 'b'. We can draw
   this graphically. The function 'map' works on lists:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 = f a1, b2 = f a2, ..., bn = f an.

   Similarly, for trees, we have, for example:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on.

   The important point to see here is that in both cases, mapping does
   not affect the *structure* of the container, only the values stored
   within it. This is an important enough concept that there is a
   special name for it. Type constructors that support an operation
   analogous to 'map' are called "Functors".

   Let's look at the type of the two mapping functions together so we
   can see how to generalise them:

       map     : (a -> b) -> [a]    -> [b]
       mapTree : (a -> b) -> Tree a -> Tree b

   The only place where these differ is the name of the container
   type. For 'map' it is '[]', meaning "lists". For 'mapTree' it is
   'Tree'. Similar in spirit to how we generalised from the specific
   to the general in Lecture 05 (Higher-Order functions), we replace
   the specific '[]' or 'Tree' with a generic 'c' to get:

       fmap   : (a -> b) -> c a -> c b

   We read this as "given a way of transforming 'a's to 'b's, we get a
   way of transforming a 'c' container full of 'a's into a 'c'
   container full of 'b's. If we replace 'c' with '[]' or 'Tree', we
   get the types of 'map' and 'mapTree' above.

   Due to the diversity of different sorts of containers, it isn't
   going to be possible to write one 'fmap' function with this
   type. Instead, we must write a separate one for each kind of
   container. Similar to how each type can have its own 'show'
   function, with a common interface described by the 'Show' typeclass
   (see Lecture 06 (Declaring Types and Classes)), we define a
   typeclass 'Functor' that describes this common interface. (The name
   'Functor' is chosen for historical reasons, one might also call it
   'Mappable'.) -}

class Functor c where
  fmap :: (a -> b) -> c a -> c b

{- As we did for 'Show', we now write "instances" of the 'Functor' type
   class for each of the container types we've seen. For lists, the
   built-in 'map' function does what we want: -}

instance Functor [] where
  fmap = map

{- For 'Tree's, we need to implement 'fmap' ourselves, since it is a
   type we defined ourselves. In Exercise 3.1.2, we ask you to
   implement 'mapTree' using 'iterTree'. Here, we define it directly
   using pattern matching: -}

instance Functor Tree where
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- 'Maybe' is also an instance of 'Functor'. Following our intuition
   that 'fmap' should not change the shape of the data, only the data
   stored within, we map 'Nothing' to 'Nothing', and 'Just' to 'Just',
   using 'f' to transform the data in the latter case: -}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

{- It is worth taking a while to look at these definitions (and the one
   for 'map') to see how they are similar, despite the different kinds
   of container.

   In every case, the constructor that is being matched on the
   left-hand side ('[]', ':', 'Leaf', 'Node', 'Nothing', 'Just') also
   appears on the right-hand side. Also, whenever there is a
   substructure (i.e., the rest of the list, the two subtrees), we
   apply 'fmap' to those as well.

   Together, these two observations indicate that our inituition about
   functors above was correct -- a 'mapping' function for a container
   preserves shapes, but transforms stored data. -}

{- It might be tempting to think that nearly every parameterised type is
   a Functor. Here is an example of a type that is not a Functor: the
   type of functions from a type to itself: -}

data Fun a = MkFun (a -> a)

{- It seems that we can write a Functor instance for this type. There is
   always a value of type 'a -> a' for any 'a': the identity function: -}

instance Functor Fun where
  fmap f (MkFun g) = MkFun id

{- This seems to be a valid definition of 'fmap' for the type
   constructor 'Fun'. But somehow it doesn't seem right: if we
   intuitively think of fmap as altering all of the values stored in a
   container whilst maintaining the structure, then it seems odd to
   always return the same answer -- the 'id' function in this
   case.

   To exclude this kind of dodgy definition, we require that 'Functor'
   instances always obey two equational laws that intuitively state
   that 'fmap' does do modification of values and not structure.

   The laws are:

      1. fmap id c == c

         Mapping the identity function over a container should not
         affect the container or its values at all. This is reasonable
         -- if we do nothing to the values stored in the container,
         then the whole thing should be unaffected.

      2. fmap f (fmap g c) == fmap (f . g) c

         If we map a function 'g' over a container, and then map a
         function 'f' over the result, then that ought to be the same
         as just mapping their composition. Again, this is reasonable:
         if we are leaving the structure of a container untouched,
         then it shouldn't matter how many times we traverse over it
         to alter the values stored in it.

   We can now see that the Functor instance for 'Fun' defined above
   fails the first law. We have:

      fmap id (MkFun g) = MkFun id

   but, to satisfy the first law, the result ought to be 'MkFun g'. -}

{- To see how thinking in terms of preserving shapes but transforming
   stored values -- thinking in Functors -- can help with structuring
   programs, we'll look again at the Process type from Exercise
   2. We'll need the 'Process' type again: -}

data Process
  = End
  | Output Bool Process
  | Input Process Process
  deriving Show

{- Let's write a new version of the 'process' function that (a) returned
   'Nothing' when there wasn't enough input, and (b) also returned any
   left over input. The 'process2' function has type: -}

process2 :: Process -> [Bool] -> Maybe ([Bool], [Bool])
{- The 'End' case signals that (a) everything is OK; and (b) that all of
   the input it is given is left over: -}
process2 End           bs     = Just ([], bs)
{- If we try to do an 'Input', but there is no input left then we signal
   an error by returning 'Nothing'. -}
process2 (Input tp fp) []     = Nothing
{- If there is some input, then we pick the appropriate choice 'tp' or
   'fp', and then carry on with processing the rest of the input. -}
process2 (Input tp fp) (b:bs) = process2 (if b then tp else fp) bs

{- The trickiest case is 'Output'. A recursive use of 'process2 p bs'
   yielded a value of type 'Maybe ([Bool], [Bool])'. To be able to
   prepend the 'Bool' being output, we had to do a pattern match. If
   processing the rest of the output gave 'Nothing', then we give
   'Nothing'. If processing the rest of the output gave 'Just'
   something, then we prepend the output: -}
process2 (Output b p)  bs     =
--   case process2 p bs of
--     Nothing       -> Nothing
--     Just (os, ls) -> Just (b:os, ls)
{- Looking at this, and comparing it to the definition of 'fmap' for
   'Maybe' above, we can see that it has the same shape: 'Nothing' is
   mapped to 'Nothing' and 'Just' is mapped to 'Just'. So, instead of
   doing the pattern matching explicitly, we could use 'fmap' to
   transform the value within the 'Maybe' container: -}
      fmap (\ (os, ls) -> (b:os, ls))
           (process2 p bs)

{- This may seem like pointless "code golf". However, it does have the
   advantage that we are no longer specific to 'Maybe' in this
   case. In Lecture 11, we will see other ways of reporting errors
   than just 'Maybe', that will also be 'Functors'. By writing our
   code using high-level functions like 'fmap', we are more able to
   change the data structures we are using without having to change
   large amounts of code. -}



{-        PART II : FOLDABLE

   The 'Functor' typeclass allows us to take a container full of 'a's
   and turn it into a container full of 'b's, provided we have a way
   of turning individual 'a's into 'b's. What else can we do with a
   container full of 'a's.

   If we have a way of combining multiple 'a's into one ("adding"
   them), then a plausible thing to do is to consider "adding up" all
   the 'a's in a container to produce a single 'a'. We'll call
   containers for which this is possible "Foldable", and we'll
   introduce a typeclass for it. Before we do that though, we have to
   nail down the idea of "addable" things.

   Let's introduce the general idea by looking at a specific container
   type: lists. Here's three ways of "adding up" everything in a list.

   First, we could take a list of lists, and append all the lists
   together, like the first question in Exercise 2: -}

concLists :: [[a]] -> [a]
concLists [] = []
concLists (xs : xss) = xs ++ concLists xss

{- Or, we could take a list of 'Int's and add them up: -}

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

{- Or, we could take a list of 'Bool's and 'and' them together,
   effectively asking ether all the booleans in the list are true: -}

allList :: [Bool] -> Bool
allList [] = True
allList (x:xs) = x && allList xs

{- Looking at these three functions, we can see that they only differ in
   two places:

     1. The "zero" thing that we use for the empty list ('[]', '0',
        and 'True', respectively); and

     2. The "operation" we use for combining results ('++', '+',
        '&&').

   We call the combination of a type with a 'zero'-like thing and an
   'add'-like thing a "monoid". We already saw this concept in Lecture
   06, and called it 'Monoid'. We make a type class to represent it: -}

class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

{- So a type 'm' is a 'Monoid' if there is an 'mempty' element of 'm',
   and an operation 'mappend' that takes two 'm's and returns an
   'm'. In the Haskell standard library, the 'Monoid' typeclass is
   already defined, but I have repeated the definition here so I can
   talk about it.

   Just as for 'fmap' in the 'Functor' type class, 'mempty' and
   'mappend' should obey some laws:

     1. For all x, y, z in 'm', it should be the case that:

           x `mappend` (y `mappend` z) == (x `mappend` y) `mappend` z

        This is usually called "associativity". It states that the
        order of doing the adding up doesn't matter (NOTE: it does
        *not* say that the order of the arguments to 'mappend' doesn't
        matter, see below.)

     2. for all x,   mempty `mappend` x == x

        This is usually called "left unit". It states that 'mempty'
        really acts like a 'zero' when combined on the left.

     3. for all x,   x `mappend` mempty == x

        This is usually called "right unit". It states that 'mempty'
        really acts like a 'zero' when combined on the right.

   Note that we don't assume the law:

       x `mappend` y == y `mappend` x

   This is usually called 'commutativity'. It does not hold for lists
   with append, which was one of our examples above. -}

{- We can now write instances for the three kinds of "adding up" that we
   used above. For lists, with append: -}

instance Monoid [x] where
  mempty  = []
  mappend = (++)

{- for 'Int's with addition: -}

instance Monoid Int where
  mempty  = 0
  mappend = (+)

{- and for 'Bool's with "and": -}

instance Monoid Bool where
  mempty  = True
  mappend = (&&)

{- Using the 'Monoid' type class, we can now write a generic function
   that "adds up" all the elements of a list, as long as those
   elements are from a type that is a "Monoid". We call this function
   'foldList', because we think of a "folding up" all the elements in
   the list, using 'mappend'. Other names for this function include
   "reduce" (as in Google's Map/Reduce) and "crush". -}

foldList :: Monoid m => [m] -> m
foldList []     = mempty
foldList (x:xs) = x `mappend` foldList xs

{- Comparing to the three functions we wrote above, 'foldList' uses
   'mempty' whenever they use the "zero"-like thing, and 'mappend'
   whenever they use the "add"-like thing. We can now recover the
   three functions above simply by giving different types to
   'foldList', which has the effect of telling Haskell which instance
   of 'Monoid' we want to use: -}

concLists' :: [[a]] -> [a]
concLists' = foldList

sumList' :: [Int] -> Int
sumList' = foldList

allList' :: [Bool] -> Bool
allList' = foldList

{- Don't be scared by the fact that all these functions have the same
   definition!. The types we have given them are enough for Haskell to
   work out what instance of "Monoid" to use, and to change the
   behaviour of "foldList" appropriately.

   Lists are not the only kind of container for which we can "add up"
   all the elements. We can also do this for 'Tree's, using 'mempty'
   for the 'Leaf's, and 'mempty' twice to add up the results of the
   left tree, the data, and the right subtree. Note that the order in
   which we do this:

         (left `mappend` x) `mappend` right, 
      or
         left `mappend` (data `mappend` right)

   doesn't matter by the first law for "Monoid"s. -}

foldTree :: Monoid m => Tree m -> m
foldTree Leaf         = mempty
foldTree (Node l x r) = foldTree l `mappend` x `mappend` foldTree r

{- We can also "add up" all the elements stored in a "Maybe"
   container. Since "Maybe"s can contain at most one thing, we don't
   need to use 'mappend', but we use 'mempty' for the "Nothing" case. -}

foldMaybe :: Monoid m => Maybe m -> m
foldMaybe Nothing  = mempty
foldMaybe (Just x) = x

{- Now we've seen three examples of containers that we can 'fold'
   over. As we did for 'Functor', let's compare their types:

      foldList  :: Monoid m => [m]     -> m
      foldTree  :: Monoid m => Tree m  -> m
      foldMaybe :: Monoid m => Maybe m -> m

   As before, these only differ in the container type. This suggests
   that it might be worth declaring a new typeclass for "Foldable"
   containers. We get the type by generalising from the specific
   containers to a generic 'c': -}

class Foldable c where
  fold :: Monoid m => c m -> m

{- Declaring instances for our -}

instance Foldable [] where
  fold = foldList

instance Foldable Tree where
  fold = foldTree

instance Foldable Maybe where
  fold = foldMaybe

{- We can now write a generic function for summing up any container full
   of integers, by specialising 'fold' to the case when the 'Monoid'
   'm' is 'Int'. This subsumes the 'sumList' function we wrote above,
   and also gives us a way to sum up 'Tree's and 'Maybe's and any
   other containers we may define in the future. -}

sum :: Foldable c => c Int -> Int
sum = fold

{- Combining 'Foldable' and 'Functor' gives us a way to preprocess
   elements before folding them up. For example, if we transform every
   element in a container to '1', and then add them up, we get a
   generic 'size' function: -}

size :: (Functor c, Foldable c) => c a -> Int
size = fold . fmap (\ _ -> 1)

{- Here, we have used the function composition operator '(.)' that we
   talked about in Lecture 6 (Higher-Order Functions).

   If we have a predicate on elements of the container, then we can
   write a generic function that checks to see whether all the
   elements of a container satisfy that predicate: -}

all :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
all p = fold . fmap p

{- The combination of 'fold' and 'fmap' is so common that the Haskell
   standard library's definition of 'Foldable' already includes
   'foldMap' as a function. This can be defined as: -}

foldMap :: (Functor c, Foldable c, Monoid m) => (a -> m) -> c a -> m
foldMap f = fold . fmap f

{- However, in some cases it might be more efficient to define a special
   'foldMap' function that does not generate an intermediate data
   structure in between the 'fmap f' and the 'fold'.

      EXERCISE: If you have a 'foldMap' for some container, can you
      always define a 'fold'?

      EXERICSE: Define a function of the type:

        toList :: Foldable c => c a -> [a]

      which shows that with 'Foldable' you can always define a
      'toList' function. If you only have a 'toList' function for a
      container can you always define 'fold'?



   One of the wrinkles of Haskell's typeclass feature is that we
   cannot define multiple instances of a typeclass for the same
   type. This would be desirable for 'Monoid' especially. Above, we
   define 'True' and '&&' as the 'mempty' and 'mappend' of the
   'Monoid' instance for 'Bool'. But this is not the only choice.

   If we want to write the 'any' function that "or"s together a list
   (dual to the "all" function that "and"s together a list), then we
   would like to write:

       instance Monoid Bool where
          mempty  = False
          mappend = (||)

   But GHC complains:

      Lec09.hs:317:10-20: Duplicate instance declarations: …
            instance Monoid Bool
               -- Defined at .../Lec09.hs:317:10
            instance Monoid Bool
               -- Defined at .../Lec09.hs:468:10

      Compilation failed.

   This makes sense: if we defined two instance of 'Monoid' for
   'Bool', then Haskell wouldn't know which one to use.

   The solution is to use Haskell's newtype feature to give 'Bool's a
   new name, which we can use to define an instance that uses 'False'
   and '||' instead of 'True' and '&&'.

   (A "newtype" is like a "data", but we can only have one constructor
    with one argument. At runtime, "newtype"s disappear completely and
    in memory this is stored simply as a normal value of 'Bool' type
    (unlike with "data", which is tagged with the constructor at
    runtime).)

   We define a new name for 'Bool', called 'Any'. The "newtype"
   declaration names the constructor we will use for converting
   'Bool's to 'Any's: -}

newtype Any = MkAny Bool

{- To convert back, we write an 'unAny' function: -}

unAny :: Any -> Bool
unAny (MkAny x) = x

{- (We could have written:

        newtype Any = MkAny { unAny :: Bool }

    as a compact way of defining 'Any', 'MkAny' and 'unAny' all in one
    go.)

   Now we can write a 'Monoid' instance for 'Any' that uses 'False'
   and '||'. This definition is a bit noisy due to the 'MkAny's
   everywhere, but it is essentially the same as the 'Monoid' instance
   for 'Bool' above that uses 'True' and '&&': -}

instance Monoid Any where
  mempty                    = MkAny False
  MkAny x `mappend` MkAny y = MkAny (x || y) 

{- Equipped with this, we can now write a generic 'exists' function that
   returns 'True' if there is any element in a container that
   satisfies the predicate: -}

exists :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
exists p = unAny . fold . fmap (MkAny . p)


{-     PART III : (EXTRA MATERIAL) THE TYPES OF TYPES

   (This section did not appear in the lecture, and is strictly for
   information only.)

   Above we define three type classes: 'Functor', 'Monoid', and
   'Foldable'. But the types involved where all slightly
   different. For 'Monoid', the instances were all "normal types" like
   'Int' or 'Bool' or '[a]'. For 'Functor' and 'Foldable', they were
   all types that take other types as parameters, like '[]' (i.e.,
   lists), 'Tree' or 'Maybe'. What's going on? How does Haskell
   distinguish between these?

   Just as values in Haskell have types, types themselves have types
   (which historically were called 'kinds'). You can get GHCi to tell
   you the type of a type by using ':k' (the 'k' standing for
   'kind'). For example:

       λ> :k Int
       Int :: *

   This tells us that the type of 'Int' is '*'. The type '*' is the
   "type of types". That is, '*' is the type of things that classify
   values. Another example of a type:

       λ> :k String
       String :: *

   Not everything that can appear in a type expression is a
   type. Let's try with one of our "container" types:

       λ> :k Maybe
       Maybe :: * -> *

   'Maybe' is not a type -- it does not have type '*'. Instead, it has
   type '* -> *', indicating that it takes a type as input, and
   returns a type. 'Maybe' is a function that operates at the type
   level. Things of this type are often called type constructors, or
   type operators. Another example, for lists:

       λ> :k []
       [] :: * -> *

   (Note that the list type constructor is written using square
   brackets with nothing inside. The notation '[Int]' is a type (i.e.,
   has type '*') and can also be written '[] Int', similar to 'Tree
   Int'.)

   Another example is the function type constructor, which has the
   following type:

       λ> :k (->)
       (->) :: * -> * -> *

   The function type constructor has two parameters: the input type
   and the output type. This is reflected in its type '* -> * -> *',
   stating that it takes a type and a type, and yields a type. There
   is potential for confusion between the '->' on the left hand side
   here, and the '->'s on the right hand side. Please remember for now
   to keep these separate! -}



{-    PART V : (EXTRA MATERIAL) THEOREMS FOR "FREE"

   (This section did not appear in the lecture, and is strictly for
   information only.)

   The intuitive separation of containers into structure and values
   stored inside the structure has the following interesting and
   useful consequence. As we have seen, 'fmap' modifies values stored
   in a structure, but not the structure itself. Conversely, functions
   that are generic in the types of elements can affect the structure,
   but not the values stored within. Examples of functions that affect
   the structure include:

   1. reverse :: [a] -> [a]

      'reverse' knows nothing about the elements stored in the list,
      so the only thing it can do is copy, discard or duplicate them
      in the output. We can know this from its type alone.

   2. mirror :: Tree a -> Tree a

      Similarly, 'mirror' knows nothing about the values of type 'a'
      stored in the tree, so all it can do is rearrange the tree.

   Since 'reverse' and 'mirror' can only affect the structure of the
   containers, and 'fmap' only affects the values in a container, it
   is possible to derive the following laws "for free":

       reverse (fmap f xs)
    ==
       fmap f (reverse xs)

   and

       mirror (fmap f t)
    ==
       fmap f (mirror t)

   Note that neither of these depend on what 'reverse' or 'mirror'
   do. Just by looking at their types, it is possible to derive these
   laws. In fact, for any functor 'c' and function 'h' of type:

       h :: c a -> c a

   It is always the case that

       h (fmap f x) == fmap f (h x)

   For more information see the paper "Theorems for Free!" by Phil
   Wadler:

       http://www.cs.sfu.ca/CourseCentral/831/burton/Notes/July14/free.pdf

   The introduction of the paper should be reasonably understandable
   if you are confident with Haskell, and gets across the general
   idea. -}
