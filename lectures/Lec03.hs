module Lec03 where

{-     LECTURE 03 : RECURSIVE FUNCTIONS I

     NOTE: this lecture is being splt into two, all the material below
     will be presented eventually, but not in this order.

   In Lecture 02 we looked at how to define functions in Haskell, and
   several techniques for reasoning our way from a specification to an
   implementation. In this lecture, we'll look in more detail at
   *recursive* functions.

   A recursive function is one that is defined in terms of
   itself. This mirrors the structure of some of the types of data we
   have looked at so far. For example, lists are defined to be either
   empty, or made from an element plus more list. To define functions
   that operate on recursively defined data, we need recursively
   defined functions.

   We have already three recursively defined functions in Lecture 02:
   'gcd', 'append', 'rev', and 'sawPrefix'. Of these, 'gcd' is in some
   ways the most complex because there was no obvious connection
   between the input data and the data passed to the next call to
   'gcd'. In contrast, the 'append' and 'rev' functions always called
   themselves on immediate sublists of the input. This kind of
   recursion "on the structure of the input" is called "structural
   recursion". Structurally recursive functions are often easier to
   understand than non-structurally recursive ones.

   In this lecture, we'll see other examples of structural vs
   non-structural recursion, and a way to turn non-structural
   recursion into structural recursion by introducing an intermediate
   data structure. -}

{-    PART I : INSERTION SORT

   Consider the problem of inserting a value into a sorted list, so
   that the resulting list is still sorted. If we assume that the
   input list is sorted in ascending order, then there are three cases
   to consider:

      1) insertion into the empty list -- we return a list with one
         element

      2) insertion into a list when the head is greater than the
         element to be inserted -- we return the new element as the
         head of the result, with the input list as the tail.

      3) insertion into a list when the head is less than the element
         to be inserted -- we return the head followed by the
         insertion of the element into the rest of the list.

   We can write this function using a mixture of pattern matching to
   look at the structure of the list and 'if-then-else's to do the
   comparisons: -}

insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) = if x <= y
                  then x : y : ys
                  else y : insert x ys

{- Note that this function is structurally recursive: when we call
   'insert' inside the definition of 'insert', we are using a value
   ('ys') we got from the input. Therefore, we can say that 'insert'
   follows the structure of its input.

   We can see how 'insert' operates by writing out a trace of how it
   works on an example list:

           insert 3 [1,4]
         =
           1 : insert 3 [4]
         =
           1 : 3 : 4 : []
         =
           [1,3,4]

   Using 'insert', we can write a sorting function by repeatedly
   inserting each element into a sorted list. Again, we can define
   this function by structural recursion on the input list: -}

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

{- The advantage of using structural recursion is that it is easier to
   reason that 'isort' always produces sorted lists which have the
   same elements as the input:

     - when the input is [], we return [], which is sorted.

     - when the input is 'x:xs', we sort 'xs', and then insert 'x'
       into the result. Since insertion into a sorted list always
       gives us a sorted list, we know that the overall result is
       sorted. Also, 'insert' inserts the element exactly once, so we
       know that the result has the same elements as the input.

   However, 'isort' has a problem, which we can see by writing out the
   trace of sorting a reversed list and writing the number of steps
   each time:

        isort [3,2,1]
       =   { 4 }
        insert 3 (insert 2 (insert 1 []))
       =   { 1 }
        insert 3 (insert 2 [1])
       =   { 2 }
        insert 3 [1,2]
       =   { 3 }
        [1,2,3]

   From this, we can see that 'isort' effectively transforms its input
   into a list of 'insert' jobs. Because the initial list was in
   reverse order, each 'insert' job has to go right to the end to
   insert its element. This means that we take a number of steps
   proportional to the square of the input list to accomplish the
   sort. Can we do better? -}

{-     PART II : QUICKSORT

   An algorithm for sorting that is, sometimes, faster than insertion
   sort is Hoare's QuickSort algorithm. QuickSort works by dividing
   the input into two large chunks and then sorting those
   independently. Therefore, it can be more efficient than insertion
   sort, which always splits the input into one very small chunk (the
   head) and the rest.

   We can write a short implementation of a simple version of
   QuickSort in Haskell. As above, sorting the empty list yields the
   empty list. To sort a list with an element 'x', we split it into
   two lists: 'smaller', which contains everything less than 'x', and
   'larger', which contains everything greater than or equal to 'x',
   then we sort those lists and stick everything back together using
   the built-in append function '++': -}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ y | y <- xs, y < x]
        larger  = [ y | y <- xs, y >= x]

{- We have used a new construct here: 'where' allows us to split out
   parts of a definition and write them separately. We could have
   written the second case of 'qsort' as:

       qsort (x:xs) = qsort [ y | y <- xs, y < x] ++ [x] ++ qsort [ y | y <- xs, y >= x]

   instead of naming the two lists 'smaller' and 'larger'. However,
   using 'where' allows us to be clearer about why we are doing
   certain things by giving them names. (It is also more efficient if
   we use the same thing more that once.)

   ASIDE: Unfortunately, this isn't a very good implementation of
   QuickSort, and some might say it is not really QuickSort at
   all. QuickSort, as originally defined by Hoare, operated on arrays
   and sorted in instead of creating (a lot of) new lists as this
   implementation does. For more informaton / opinions, see:

      https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort

   Also the way that the pivot element is selected in this
   implementation is very naive, and can often yield the same worst
   case time behaviour as insertion sort. Nevertheless, it is a good
   example of a non-structurally recursive function for our purposes.

   END OF ASIDE.

   The definition of 'qsort' is all very well, but it is not
   structurally recursive. We call 'qsort' recursively on lists that
   are computed via a (relatively) complex list comprehension, and not
   just ones that are discovered by pattern matching. This makes it
   harder to see that 'qsort' is definitely doing the right
   thing.

   To help us see what is going on inside 'qsort', let's step through
   an example:


     qsort [5,3,1,2]
   =
     qsort [3,1,2]                                               ++ [5] ++ qsort []
   =
     (qsort [1,2]                            ++ [3] ++ qsort []) ++ [5] ++ []
   =
     (qsort [1,2]                            ++ [3] ++ qsort []) ++ [5] ++ []
   =
     ((qsort [] ++ [1] ++ qsort [2])         ++ [3] ++ [])       ++ [5] ++ []
   =
     (([]       ++ [1] ++ ([] ++ [2] ++ [])) ++ [3] ++ [])       ++ [5] ++ []
   =
     [1,2,3,5]

   We have formatted this example to reveal some of the internal
   structure of the tasks that 'qsort' generates. Looking at the final
   structure of the appends ('++'s) at the end, we can see that there
   is a tree structure:

                       [5]
                  [3]      []
              [1]    []
            []  [2]
               [] []

   Let's now see how to reformulate 'qsort' in terms of intermediate
   tree data structure, which will help us make a structurally
   recursive variant. -}

{-    PART III : TREESORT

   We want to represent binary trees, so we create a new data type for
   this purpose. We name this data type 'BST' for Binary Search Tree
   to indicate that we want it to have a special property with respect
   to sortedness. Specifically, a tree is a binary search tree if:

       1. it is 'Leaf'; or

       2. it is 'Node l x r' and all of the following are true:
          (a) every value in l is <= x
          (b) every value in r is >= x
          (c) l is a binary search tree
          (d) r is a binary search tree
-}

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving Show

{- We will build up our 'BST's by inserting elements into them,
   maintaining the properties listed above. This insertion function is
   analogous to the 'insert' function on lists we defined above. As
   above, there are three cases:

       1. The tree is empty: we make a new tree with a single node;

       2. The element at the root of the tree is less than the element
          we want to insert: we insert the element into the left hand
          (smaller) subtree;

       3. The element at the root of the tree is greater than or equal
          to the element we want to insert: we insert the element into
          the right hand (larger) subtree.

   We write out these cases using pattern matching and guards. As with
   the 'insert' function above, this function is structurally
   recursive and we can check for each case that it (a) always returns
   a Binary Search Tree; and (b) the values in the result tree are all
   the values in the input tree, plus the new value. -}

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node smaller y larger)
  | x < y     = Node (insertBST x smaller) y larger
  | otherwise = Node smaller y (insertBST x larger)

{- As we saw above, 'qsort' operates by converting the input list into a
   tree of jobs to perform. We copy this idea by writing a (structurally
   recursive) function to convert a list to a tree by repeated
   insertion: -}

listToTree :: Ord a => [a] -> BST a
listToTree []     = Leaf
listToTree (x:xs) = insertBST x (listToTree xs)

{- We can see how this generates the same trees as qsort (after
   reversal, because it builds up the tree from the last element to
   the first):

     > listToTree (reverse [5,3,1,2])
     Node (Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 Leaf) 5 Leaf

   This is exactly the Haskell representation of the qsort tree we
   drew above.

   Now, to convert a tree to a list, we 'flatten' it. You already saw
   this function in Exercise 1. We work on the structure of the tree,
   converting leaves to empty lists, and converting nodes to the
   concatenation of the smaller, middle bit, and larger parts: -}

flatten :: BST a -> [a]
flatten Leaf = []
flatten (Node smaller a larger) =
  flatten smaller ++ [a] ++ flatten larger

{- Finally, we can put 'flatten' and 'listToTree' together to get the
   'treesort' function: -}

treesort :: Ord a => [a] -> [a]
treesort xs = flatten (listToTree xs)


{-    PART IV : FLATTEN WITH AN ACCUMULATOR

   Unfortunately, the 'flatten' function defined above is not
   particularly efficient. It uses list append ('++') to create the
   output list. Let's recall from Lecture 03 how list append is
   defined. It is defined by structural recursion on its first
   argument:

        append :: [a] -> [a] -> [a]
        append [] ys = ys
        append (x:xs) ys = x : (append xs ys)

   This means that the number of steps required to perform an append
   is equal to the length of the first list. Since the result of a
   'flatten' that is invoked by another 'flatten' may result in going
   over the same elements over and over again, just as in the repeated
   inserts having travel the whole length of the list as we saw above.

   A standard technique for making this kind of function faster is to
   use a separate 'accumulator' argument. Instead of repeatedly
   traversing the input to build the output, we incrementally build
   the output by passing partial output into the function and
   returning the updated partial output.

   Here is 'flatten2', 'flatten' written using an accumulator. We pass
   in a list that contains the partially constructed output, which we
   call 'acc'. In the 'Leaf' case, there is nothing to add, so we
   return 'acc'. In the 'Node' case, we (a) add the element from
   'larger'; (b) add the element 'x'; and (c) add the elements from
   smaller: -}

flatten2 :: BST a -> [a] -> [a]
flatten2 Leaf acc = acc
flatten2 (Node smaller x larger) acc =
  flatten2 smaller (x:flatten2 larger acc)

{- We will see more examples of using accmulators in the next
   Tutorial. In the meantime, see if you can work out how 'flatten2'
   works by writing out how it operates on an example tree.

   We can use 'flatten2' as a drop-in replacement in treesort, as long
   as we remember to pass in the empty list as the initial partial
   output: -}

treesort2 :: Ord a => [a] -> [a]
treesort2 xs = flatten2 (listToTree xs) []
