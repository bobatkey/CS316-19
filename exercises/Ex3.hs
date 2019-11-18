{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex3 where

import           Data.Char (isSpace, isLower, isUpper, isNumber,
                            digitToInt, isAlpha, isAlphaNum)
import           Data.List (intersperse)
import qualified Data.Map as M
import           Data.Map (Map)

{----------------------------------------------------------------------}
{- CS316 (2019/20) EXERCISE 3                                         -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 2pm on Monday 2nd
   December.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   40% of the overall marks for the class (so one mark, below is worth
   half a percent).

   The test will consist of another file which will import this
   file. You will need to answer the questions in that file, and
   commit both by the end of the lab session on Monday 2nd December.

   Note about plagiarism: For the take home parts of this exercise,
   you can discuss the questions with others to make sure that you
   understand the questions. However, you must write up your answers
   by yourself. For the lab tests, no conferring is
   allowed. Plagiarism will be taken very seriously. -}


{----------------------------------------------------------------------}
{- GHOUL : Global Higher-order Untyped Language                       -}
{----------------------------------------------------------------------}

{- INTRODUCTION TO GHOUL

   This exercise will lead you through building an interpreter for a
   small functional language called "GHOUL". It will bring together
   all of the concepts you have learned during this course.

   Here is an example GHOUL program (written using 'unlines' to allow
   multiple lines). It implements addition of positive whole numbers,
   where the number 'n' is represented as 'n' applications of a
   constructor 'S' to 'Z'. -}

plusProgram :: String
plusProgram = unlines
  [ "(plus Z     y) = y"
  , "(plus (S x) y) = (S (plus x y))"
  , "(main)         = (plus (S (S Z)) (S (S Z)))"
  ]

{- Each line of a GHOUL program defines an equation defining what
   happens if that function symbol is invoked with those arguments. As
   in Haskell, capital letters indicate constructors, and lower case
   letters indicate variables and function names.

   In 'plusProgram', the 'main' function expects no arguments, and
   runs 'plus' with the arguments '(S (S Z))' and '(S (S Z))' (i.e., 2
   and 2).

   Execution of GHOUL programs works by starting from the 'main'
   function and then matching each function application against the
   patterns defined for that function to get a right-hand side
   expression to replace that application with. This continues until
   there are no more expressions to replace. Data is built from
   constructors (identifiers that start with capital letters) applied
   to other constructors.

   For our example program, we have:

         (main)
      =  (plus (S (S Z)) (S (S Z)))
      =  (S (plus (S Z) (S (S Z))))
      =  (S (S (plus Z (S (S Z)))))
      =  (S (S (S (S Z))))

   So "two plus two is four".

   GHOUL is similar to Haskell, except that there are no types and
   there are no lambda expressions (\x -> ...).

   In this exercise, you will build a GHOUL interpreter, and extend it
   with additional features.

   In general, all interpreters perform the following steps:

     1. Parse the input -- take a representation of a program as a
        list of characters and turn it into abstract syntax. You will
        do this using parser combinators (Lectures 14 and 15) in
        Section 4.

     2. Post-process and check the input, often called "Elaboration"
        or "Type Checking". In Haskell this is a complex stage
        involving desugaring, type inference, and typeclass
        resolution. For GHOUL, you will write some code to perform
        some well-formedness checks on the program.

     3. Execute the program. In simple interpreters like this one,
        this works by pattern matching on the expressions, performing
        the appropriate action for each kind of expression. In GHOUL,
        the expressions consist of variables, function applications,
        and constructor applications.

   These steps are summed up by the following function that parses,
   checks, and executes GHOUL programs: -}

runGHOUL :: String -> ErrorOr (ErrorOr Value)
runGHOUL text = do
  prog <- parse pProgram text
  checkProgram prog
  return (evalProgram prog)


{- 'runGHOUL' accepts a String containing a GHOUL program, parses it,
   checks it, and executes it. If any of these steps fail, an error
   message is returned. Otherwise, the result of evaluating the
   program is returned.

   Of course, 'runGHOUL' doesn't work yet -- you will need to fill in
   the details below.

   Errors are tracked by using the datatype 'ErrorOr', which is used
   to simulate exceptions similar to how we used 'Maybe' in Lecture
   10. The result of a 'ErrorOr' computation is either 'OK x' with
   some value 'x', or 'Error msg' where 'msg' is some hopefully
   helpful error message. -}

data ErrorOr a
  = Ok a
  | Error String
  deriving Show

{- The 'ErrorOr' type is an instance of 'Monad' (Lecture 12), 'Functor'
   (Lecture 09), and 'Applicative' (Lecture 16). The code implementing
   these interfaces in the appendix below. Apart from these
   interfaces, 'ErrorOr' gives us a way of reporting errors: -}

abortWithMessage :: String -> ErrorOr a
abortWithMessage s = Error s

{- And a way of trying one computation, and if that fails, another
   computation. This is similar to the 'catch' function from Lecture
   10. -}

onError :: ErrorOr a -> ErrorOr a -> ErrorOr a
onError (Ok x)    _   = Ok x
onError (Error _) op2 = op2

{- When you've written at least the parser and evaluator parts, you
   should be able to use 'runGHOUL' to run a GHOUL program:

         λ> runGHOUL plusProgram
         OK (OK (VC "S" [VC "S" [VC "S" [VC "S" [VC "Z" []]]]]))

   This exercise is structured so that you can implement a basic GHOUL
   evaluator first, and then go back to extend it with additional
   features for extra marks. As with the previous exercises, roughly a
   third of the marks will only be available during the class test on
   Monday 2nd December. -}

{----------------------------------------------------------------------}
{- Part 0 : ABSTRACT SYNTAX                                           -}
{----------------------------------------------------------------------}

{- Before we can write an interpreter for GHOUL programs, we need to
   describe what the syntax of GHOUL programs is. Generalising from
   the 'plus' example above, a GHOUL program is:

     - a list of rules, such as:

           (plus Z y)     = y
           (plus (S x) y) = (S (plus x y))
           (main)         = (plus (S (S Z)) (S (S Z)))

       where:

     - a rule is a name and a list of patterns, surrounded by
       parentheses, followed by an equals '=' and then an
       expression. For example:

                 (plus Z y)     = y
            or   (plus (S x) y) = (S (plus x y))
            or   (main)         = (S (S Z))
            or   (main)         = (Cons (S Z) (Cons Z Nil))

       where:

     - a pattern is a variable name (first letter lower case) or a
       constructor name (first letter upper case) on its own, or a
       constructor name followed by a space separated list of
       patterns, surrounded by parentheses. Examples:

                 Z
            or   (S Z)
            or   x
            or   (Cons x y)

       and

     - an expression is either a variable name, a constructor name, or
       an application of a function to space separated list of
       expressions, all surrounded by parentheses, or an application
       of a constructor name to expressions, all surrounded by
       parentheses. For example:

                 (S (plus x y))
            or   (plus x y)
            or   Z
            or   x

   Also, there must be a rule named 'main' that has no argument
   patterns:

                 (main) = (plus Z Z)
            or   (main) = (plus (plus (S Z) (S Z)) (S Z))

   Following this description, we represent GHOUL programs as values
   of the type 'Program', where a 'Program' is a list of rules. -}

type Program = [Rule]

{- As we mentioned above, rules

        (plus Z y) = y

   consist of:

    1. a name (e.g., "plus", "main")

    2. a list of patterns (patterns are defined below)

    3. an expression for the right hand side (expressions are defined
       below).

   We write a type for representing equations like so: -}

data Rule = MkRule String [Pat] Exp
  deriving (Show, Eq)

{- A pattern is either a variable (PV), or a constructor name and a list
   of patterns (PC). This is similar to patterns in Haskell, except
   that GHOUL does not have a "catch all" pattern '_'. -}

data Pat
  = PV String
  | PC String [Pat]
  deriving (Show, Eq)

{- Here are some example 'Pat's:

   - A GHOUL variable pattern 'y' is represented by the Haskell value
     'PV "y"'.

   - A GHOUL pattern matching a constructor with no arguments,
     e.g. 'Nil', is represented by the Haskell value 'PC "Nil" []'.

   - A GHOUL pattern matching a constructor with some arguments,
     e.g. '(Cons x Nil)', is represented by the Haskell value
     'PC "Cons" [PV "x", PC "Nil" []]'.

   An expression is either a variable (EV), an application of a named
   function (EA) or a an application of a constructor (EC). -}

data Exp
  = EV String
  | EA String [Exp]
  | EC String [Exp]
  deriving (Show, Eq)

{- Here are some example 'Exp's:

   - A GHOUL variable 'x' is represented by the Haskell value
     'EV "x"'.

   - A GHOUL function application '(f x y)' is represented by the
     Haskell value 'EA "f" [EV "x", EV "y"]'.

   - A GHOUL constructor with no arguments, e.g. 'Nil' is represented
     as 'EC "Nil" []'.

   - A GHOUL constructor with some arguments, e.g., '(Cons x Nil)' is
     represented as 'EC "Cons" [EV "x", EC "Nil" []]'.

   Note that in the GHOUL syntax, patterns are a subset of
   expressions, but when we represent them in Haskell they are in two
   separate data types. The job of working out which bits of a GHOUL
   program are patterns and which bits are expressions is done by the
   parser you will define below in Section 4.

   Here are some example 'Rule's:

   - A GHOUL rule with no arguments:

         (main) = (plus Z Z)

     is represented by the Haskell value:

         MkRule "main" [] (EA "plus" [EC "Z" [], EC "Z" []])

   - A GHOUL rule with two arguments:

         (eq Z Z) = True

     is represented by the Haskell value:

         MkRule "eq" [PC "Z" [], PC "Z" []] (EC "True" [])
-}

{- AN EXAMPLE Abstract Syntax Tree

   Here is an example 'Program', representing the example program
   'plus' we saw above. -}

plusProgramAST :: Program
plusProgramAST =
  [ MkRule "plus"
           [PC "Z" [], PV "y"]
           (EV "y")
  , MkRule "plus"
           [PC "S" [PV "x"], PV "y"]
           (EC "S" [EA "plus" [EV"x", EV"y"]])
  , MkRule "main"
           []
           (EA "plus" [EC "S" [EC "S" [EC "Z" []]],
                       EC "S" [EC "S" [EC "Z" []]]])
  ]

{- Make sure you understand the correspondence between the Haskell value
   'plusProgramAST' and the concrete syntax in the 'plusProgram'
   variable defined above before proceeding. -}


{- PRETTY PRINTING

   Reading the raw Haskell representation of the ASTs is a bit hard,
   so I've written a pretty printer that converts GHOUL programs to
   their concrete syntax.

   The functions 'ppProgram', 'ppRule', 'ppPat', and 'ppExp' turn the
   corresponding bit of abstract syntax into their string
   representation. Use the function 'printProgram' to print out whole
   programs. For example:

     > printProgram plusProgramAST
     (plus Z y) = y
     (plus (S x) y) = (S (plus x y))
     (main) = (plus (S (S Z)) (S (S Z)))

   Notice that it matches (ignoring differences in spaces) the
   plusProgram 'String' above. -}


printProgram :: Program -> IO ()
printProgram prog = putStr (ppProgram prog)

ppProgram :: Program -> String
ppProgram = unlines . map ppRule

ppRule :: Rule -> String
ppRule (MkRule fname [] exp) =
  ppParens fname ++ " = " ++ ppExp exp
ppRule (MkRule fname pats exp) =
  ppParens (fname ++ " " ++ ppSpacedList ppPat pats) ++ " = " ++ ppExp exp

ppPat :: Pat -> String
ppPat (PV vname)      = vname
ppPat (PC cname [])   = cname
ppPat (PC cname pats) = ppParens (cname ++ " " ++ ppSpacedList ppPat pats)

ppExp :: Exp -> String
ppExp (EV vname)      = vname
ppExp (EA fname [])   = ppParens fname
ppExp (EA fname exps) = ppParens (fname ++ " " ++ ppSpacedList ppExp exps)
ppExp (EC cname [])   = cname
ppExp (EC cname exps) = ppParens (cname ++ " " ++ ppSpacedList ppExp exps)

{- The following two functions are helper functions for constructing
   strings surrounded by parentheses, and for constructing strings of
   pretty printed things separated by spaces. They are used by the
   functions above to avoid repeated code. -}

ppParens :: String -> String
ppParens s = "(" ++ s ++ ")"

ppSpacedList :: (a -> String) -> [a] -> String
ppSpacedList pp = concat . intersperse " " . map pp




{- 3.0.0 Write a GHOUL program to concatenate lists.

   Write a GHOUL program to concatenate (append) two lists as a value
   of type 'Program'. Remember that to be a valid GHOUL program, you
   should also include a 'main' function definition. Use the example
   of the 'append' function written in Haskell using the 'Cons' and
   'Nil' constructors in Lecture 01 as a guide. -}

appendProgramAST :: Program
appendProgramAST = undefined -- Fill this in

{- 3 MARKS -}

{----------------------------------------------------------------------}
{- Part 1 : VALUES and ENVIRONMENTS                                   -}
{----------------------------------------------------------------------}

{- As described above, execution of GHOUL programs proceeds by matching
   patterns in the rules against values computed during th
   execution. Runtime values are the results of executing
   'Exp'ressions.

   Runtime values are a subset of 'Exp's, restricted to just
   constructors and constructors applied to other
   constructors. Evaluation is the process of filling in variables and
   evaluating functions. -}

data Value
  = VC String [Value]
  deriving (Eq, Show)

{- For example, the expression '(S Z)' evaluates to the 'Value':

      VC "S" [VC "Z" []]
-}



{- 3.1.0 Implement a "pretty printer" for GHOUL values, that prints them
   out in the same syntax as an expression that would generate them. -}

ppValue :: Value -> String
ppValue = undefined

{- HINT: you will find the functions 'ppSpacedList' and 'ppParens'
   defined above useful. -}

{- 2 MARKS -}



{- GHOUL programs have variables in them. To keep track of what each
   variable means by during execution, we use environments. An
   environment is a 'Map', associating values to names: -}

type Env = Map String Value

{- The 'Map' data type is provided by the Data.Map module imported at
   the start of this file. The 'Map' type takes two arguments: the
   first is the type of keys (here 'String's representing names), and
   the second is the type of values (here 'Value', representing GHOUL
   values).

   The Data.Map module provides a rich interface for querying and
   updating 'Map's. The module has been imported as 'qualified M',
   meaning that the functions from this module need to be prefixed by
   'M.' to be used. This is to avoid name clashes.

   For this exercise, you will need the following functions that build
   and manipulate 'Map's (use ':t <functioname>' in GHCi to look at
   the types as well):

      M.empty           -- the empty Map

      M.lookup k m      -- looks up the value of the key 'k' in 'm'.
                           Returns 'Just v' if the value 'v' is
                           associated with 'k' in 'm'. Otherwise,
                           returns 'Nothing'.

      M.insert k v m    -- returns a new map that has the same key->value
                           mapping as 'm' except that now 'k' has the
                           value 'v'.

      M.fromList kvs    -- constructs a 'Map' from a list 'kvs' of
                           (key,value) pairs. If a key is repeated, then
                           the last value is taken.

   For example, the empty environment (no variables have values) is the
   empty map:

        M.empty
     or
        M.fromList []

   An environment that assigns the GHOUL value 'Z' to 'x' and '(S Z)'
   to 'y' is the list:

        M.insert "x" (VC "Z" []) (M.insert "y" (VC "S" [VC "Z" []]) M.empty)
     or
        M.fromList [ ("x", VC "Z" [])
                   , ("y", VC "S" [VC "Z" []])
                   ]

    We look up the values assigned to variables in an environment
    using the 'M.lookup' function:

       > let env = M.fromList [ ("x", VC "Z" []), ("y", VC "S" [VC "Z" []]) ]
       > M.lookup "x" env
       Just (VC "Z" [])
       > M.lookup "z" env
       Nothing
       > M.lookup "z" (M.insert "z" (VC "Nil" []) env)
       Just (VC "Nil" [])
-}

{- 3.1.1 Binding Variables

   Implement the function 'bindVar' that acts like 'M.insert' except
   that a call:

          bindVar x value env

   acts as follows:

     - if the variable 'x' is already in the environment, then it
       returns 'Error' with a suitable error message.

     - if the variable 'x' is not in the environment, it inserts it
       into 'env' to get 'newEnv' and returns 'Ok newEnv'.

   You will need to use 'M.insert' to write 'bindVar'.

   Examples:

         bindVar "x" (VC "Z" []) M.empty
      == Ok (M.fromList [("x", VC "Z" [])])

         bindVar "x" (VC "Z" []) (M.insert "x" (VC "Nil" []) M.empty)
      == Error <error message>
-}

bindVar :: String -> Value -> Env -> ErrorOr Env
bindVar = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- Part 2 : PATTERN MATCHING                                          -}
{----------------------------------------------------------------------}

{- As described above, execution of a GHOUL program alternates between
   selecting rules, and then evaluating the right-hand side of that
   rule.

   Selecting rules is accomplished by pattern matching. Matching a
   pattern against a value updates an environment to bind variables in
   the pattern to values. For example (using GHOUL syntax instead of
   abstract syntax for brevity):

    - Matching the pattern 'x' against any value 'v' will bind 'x' to
      'v'.

    - Matching the pattern '(S x)' against the value '(S Z)' will bind
      'x' to the value 'Z'.

    - Matching the pattern '(S x)' against the value 'Z' will fail,
      because the constructors 'S' and 'Z' do not match. -}


{- 3.2.0 Matching patterns against values.

   Write the functions 'matchPattern' and 'matchPatterns'.

   'matchPattern' takes a pair of a pattern and a value, and an
   environment and returns a possibly updated environment. The
   function should implement pattern matching:

     - matching a variable against any value binds the variable to the
       value (using 'bindVar').

     - matching a constructor pattern against a value checks that the
       value has a constructor with the same name, and that the
       sub-patterns match all of the sub-values.

   'matchPats' should take a list of patterns and a list of values,
   and return a Matcher generated by matching all the pairs. If the
   two lists have different lengths, then matching ought to fail. Use
   the 'zipChecked' function we gave you above, which checks that two
   lists have the same length.

   Some test cases:

     > matchPattern (PV "x") (VC "S" [VC "Z" []]) M.empty
     Ok (fromList [("x",VC "S" [VC "Z" []])])

     > matchPattern (PC "S" [PV "x"]) (VC "S" [VC "Z" []]) M.empty
     Ok (fromList [("x",VC "Z" [])])

     > matchPattern (PC "S" [PC "Z" []]) (VC "S" [VC "Z" []]) M.empty
     Ok (fromList [])

     > matchPattern (PC "Cons" [PV "x", PV "y"]) (VC "Cons" [VC "A" [], VC "B" []]) M.empty
     Ok (fromList [("x",VC "A" []),("y",VC "B" [])])

     > matchPattern (PC "Cons" [PV "x", PV "y"]) (VC "NotCons" [VC "A" [], VC "B" []]) M.empty
     Error <error message>

     > matchPatterns [PV "x", PC "S" [PC "Z" []]] [VC "Z" [], VC "S" [VC "Z" []]] M.empty
     Ok (fromList [("x",VC "Z" [])])

     > matchPatterns [PV "x", PC "S" [PV "x"]] [VC "Z" [], VC "S" [VC "Z" []]] M.empty
     Error <error message>

     > matchPatterns [] M.empty
     Ok (fromList [])
-}

matchPattern :: Pat -> Value -> Env -> ErrorOr Env
matchPattern = undefined

matchPatterns :: [Pat] -> [Value] -> Env -> ErrorOr Env
matchPatterns = undefined

{- 4 MARKS -}

{- 3.2.1 Finding Rules by Pattern Matching

   Write a function that, given a name and a list of values, searches
   a 'Program' for the first rule that matches. That is, the names
   should match, and the patterns of the equation should match the
   list of values. On success, you should return the expression
   associated with that rule and the environment built from doing the
   pattern match.

   One way to write this function is to use the 'onError' function to
   try one rule, and if that fails, try another.

     > findRule "plus" [VC "Z" [], VC "Z" []] plusProgramAST
     Ok (fromList [("y",VC "Z" [])],EV "y")

     > findRule "plus" [VC "S" [VC "Z" []], VC "Z" []] plusProgramAST
     Ok (fromList [("x",VC "Z" []),("y",VC "Z" [])],EC "S" [EA "plus" [EV "x",EV "y"]])

     > findRule "plus" [VC "Nil" [], VC "Nil" []] plusProgramAST
     Error "..."

     > findRule "otherFunctionName" [VC "Nil" [], VC "Nil" []] plusProgramAST
     Error "..."

     > findRule "main" [] plusProgramAST
     Ok (fromList [],EA "plus" [EC "S" [EC "S" [EC "Z" []]],EC "S" [EC "S" [EC "Z" []]]])
-}

findRule :: String -> [Value] -> Program -> ErrorOr (Env, Exp)
findRule = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- Part 3: EVALUATION OF EXPRESSIONS                                  -}
{----------------------------------------------------------------------}

{- Evaluation of expressions in the context of some program is modelled
   using the 'Eval' data type. The 'Eval' type offers two services as
   well as being a 'Monad':

     a) The ability to look at the current program ('currentProgram')

     b) The ability to report failed execution ('abortEval'). -}

newtype Eval a =
  MkEval (Program -> ErrorOr a)

{- To 'run' some evaluation, we use the 'runEval' function that runs an
   evaluation with a given program and returns either an error or a
   value: -}

runEval :: Eval a -> Program -> ErrorOr a
runEval (MkEval e) program = e program

{- 'Eval' supports the Monad operations 'return' and '>>='. As a
   consequence it also supports the 'Functor' (Lecture 09) and
   'Applicative' interfaces (Lecture 16): -}

instance Monad Eval where
  return x = MkEval (\prg -> return x)

  e >>= k = MkEval (\prg -> do a <- runEval e prg
                               runEval (k a) prg)

instance Functor Eval where
  fmap f ea = do a <- ea; return (f a)

instance Applicative Eval where
  pure = return
  ef <*> ea = do f <- ef; a <- ea; return (f a)

{- The three basic operations supported by the 'Eval' monad are the ones
   that abort evaluation with an error message ('abortEvaluation'),
   access the program being executed ('getCurrentProgram'), and lift a
   computation that may error (an 'ErrorOr' computation) up to a
   'Eval' computation. -}

abortEvaluation :: String -> Eval a
abortEvaluation msg = MkEval (\prg -> abortWithMessage msg)

getCurrentProgram :: Eval Program
getCurrentProgram = MkEval (\prg -> Ok prg)

liftError :: ErrorOr a -> Eval a
liftError e = MkEval (\prg -> e)

{- 3.3.0 Expression evaluation

   Write the 'eval' function. This function takes two arguments:

     - The 'Env'ironment that describes what values are assigned to
       what variables.

     - The 'Exp'ression to evaluate.

   It returns a computation in the 'Eval' monad.

   Evaluation proceeds like so:

     - If the expression is a variable 'EV var', then look up that
       variable in the environment. If it is not there then abort with
       an appropriate error message.

     - If the expression is a constructor 'EC c args', then evaluate
       all the arguments (you might find the 'mapM' function useful
       here). Then return 'VC c' applied to the result of evaluating
       all the 'args'.

     - If the expression is a function application 'EA f args', first
       evaluate all the arguments to values. Then get the current
       program and use 'findRule' to find a rule that matches the
       values computed from the arguments and the updated
       environment. Then evaluate the right hand side of that rule in
       the environment generated by pattern matching.

   Some test cases:

    > runEval (eval (M.fromList [("x", VC "Z" [])]) (EV "x")) plusProgramAST
    Ok (VC "Z" [])

    > runEval (eval (M.fromList [("x", VC "Z" [])]) (EV "x")) []
    Ok (VC "Z" [])

    > runEval (eval (M.fromList [("x", VC "Z" [])]) (EC "Box" [EV "x"])) []
    Ok (VC "Box" [VC "Z" []])

    > runEval (eval (M.fromList [("x", VC "Z" [])]) (EC "Box" [EC "Blob" []])) []
    Ok (VC "Box" [VC "Blob" []])

    > runEval (eval M.empty (EC "Box" [EC "Blob" []])) []
    Ok (VC "Box" [VC "Blob" []])

    > runEval (eval M.empty (EA "plus" [EC "S" [EC "Z" []], EC "S" [EC "Z" []]])) []
    Error "..."

    > runEval (eval M.empty (EA "plus" [EC "S" [EC "Z" []], EC "S" [EC "Z" []]])) plusProgramAST
    Ok (VC "S" [VC "S" [VC "Z" []]])
-}

eval :: Env -> Exp -> Eval Value
eval = undefined

{- 5 MARKS -}



{- Once you have implemented 'eval', the following function to evaluate
   whole programs will work. This is used by runGHOUL. -}

evalProgram :: Program -> ErrorOr Value
evalProgram program =
  runEval                         -- run an evaluation
    (eval M.empty (EA "main" [])) -- of the expression that calls the
                                  -- 'main' function with no arguments
    program                       -- in the given program

{- You can now run the 'plus' program we saw at the start:

     > evalProgram plusProgramAST
     Ok (VC "S" [VC "S" [VC "S" [VC "S" [VC "Z" []]]]])

   Running a program with just a 'main' function is also possible:

     > evalProgram [MkRule "main" [] (EC "Nothing" [])]
     Ok (VC "Nothing" [])
-}


{----------------------------------------------------------------------}
{- Part 4 : PARSING                                                   -}
{----------------------------------------------------------------------}

{- Writing GHOUL programs as values of type 'Program' is all very well,
   but not very friendly.

   Instead, we will build a parser and elaborator that will take a
   'String' that represents a GHOUL program and turn it into a list of
   rules.

   You will build your parser using parser combinators, as introduced
   in Lectures 14 and 15. Unlike in the lectures, we will write
   parsers that produce error messages rather than just returning
   'Nothing' on failure. -}

newtype Parser a = MkParser (String -> ErrorOr (a, String))

{- Parsers are applied to 'String's by using the 'runParser' function,
   which returns the value parsed, and the left over input: -}

runParser :: Parser a -> String -> ErrorOr (a,String)
runParser (MkParser p) = p

{- 'runParser' is useful for testing parsers.

   To parse a complete string all the way to the end, we use 'parse',
   which checks that the whole input has been parsed, by checking that
   the leftover string is empty. -}

parse :: Parser a -> String -> ErrorOr a
parse p input =
  case runParser p input of
    Ok (a, "")       -> Ok a
    Ok (_, leftover) -> Error ("Unexpected trailing input: '" ++ leftover ++ "'")
    Error e          -> Error e

{- The rest of the parser combinator functions are at the end of this
   file. The main combinators that you will want to use to build your
   parsers are the following (these were all discussed in Lectures 14
   and 15):

     - The Functor, Applicative, and Monad interfaces.
     - 'failParse' to report failure.
     - 'orElse' to try one parser and, if that fails, another one.
     - 'isChar' to parse given characters.
     - 'string' is parse given strings.
     - 'identifier' to parse identifiers: sequences of letters and numbers
       that must start with a letter.
     - 'spaces' to parse zero or more white space characters.
     - 'sepBy' to parse lists of things separated by something.

   To begin the GHOUL parser, you will first construct two parsers
   that recognise variable names and constructor names. We will use
   these later on as part of our pattern and expression parsers. -}

{- 3.4.0 Write a 'Parser' for variable names.

   Follow the Haskell convention that a variable name is an identifier
   that starts with a lower case letter. Use the library function
   'isLower' to identify lower case letters.

   HINT: Use the parser 'identifier' defined below to parse
   identifiers, and then check to see whether or not it is a valid
   variable name. -}

pVariableName :: Parser String
pVariableName = undefined

{- Here are some tests that your 'pVariableName' parser should pass:

     runParser pVariableName "plus"  == Ok ("plus", "")
     runParser pVariableName "x"     == Ok ("x", "")
     runParser pVariableName "Plus"  == Error <error message>
     runParser pVariableName ""      == Error <error message>
     runParser pVariableName "plu s" == Ok ("plu", " s")
     runParser pVariableName "123"   == Error <error message>

  Note that the tests do not specify what error messages look
  like. That is up to you. -}

{- 1 MARK -}


{- 3.4.1 Write a 'Parser' for constructor names.

   Follow the convention that a constructor name is an identifier that
   starts with an upper case letter. Use the library function
   'isUpper' to identify upper case letters. -}

pConstructorName :: Parser String
pConstructorName = undefined

{- Here are some tests that your 'pConstructorName' parser should pass:

     runParser pConstructorName "plus"  == Error <error message>
     runParser pConstructorName "x"     == Error <error message>
     runParser pConstructorName ""      == Error <error message>
     runParser pConstructorName "Plus"  == Ok ("Plus", "")
     runParser pConstructorName "S"     == Ok ("S", "")
     runParser pConstructorName "plu s" == Error <error message>
     runParser pConstructorName "123"   == Error <error message> -}

{- 1 MARK -}


{- 3.4.2 Parsing patterns.

   A pattern is either:

     - a variable name; or
     - a constructor name followed by a whitespace separated list of
       patterns, all surrounded by parentheses; or
     - a constructor name.

   For example:

         (Cons Z xs)

   Write a parser for patterns, using the parser combinators. -}

pPat :: Parser Pat
pPat = undefined

{- Here are some tests that your 'pat' parser should pass:

     runParser pPat "x"       == Ok (PV "x","")
     runParser pPat "Z"       == Ok (PC "Z" [],"")
     runParser pPat "(S x)"   == Ok (PC "S" [PV "x"],"")
     runParser pPat "(S x y)" == Ok (PC "S" [PV "x",PV "y"],"")
     runParser pPat ""        == Error <error message>
     runParser pPat "S x"     == Ok (PC "S" []," x")
     runParser pPat "x(x,y)"  == Ok (PV "x","(x,y)")

   Note the last two cases: they have only parsed part of the input,
   and returned the bit they couldn't parse. -}

{- 3 MARKS -}


{- 3.4.3 Parsing expressions

   An expression is either:

     - a variable name followed by a space separated list of
       expressions, all in parentheses, which is interpreted
       as a function call; or
     - a constructor name followed by a space separated list of
       expressions, all in parentheses; or
     - a variable name; or
     - a constructor name.

   For example:

        (append (Cons Z Nil) xs)

   Write a parser for expressions. -}

pExp :: Parser Exp
pExp = undefined

{- Some test cases:

     > runParser pExp "x"
     Ok (EV "x","")

     > runParser pExp "()"
     Error <message>

     > runParser pExp "(f)"
     Ok (EA "f" [],"")

     > runParser pExp "(f x y)"
     Ok (EA "f" [EV "x",EV "y"],"")

     > runParser pExp "(f X Y)"
     Ok (EA "f" [EC "X" [],EC "Y" []],"")

     > runParser pExp "(f (S Z) (Cons A Nil))"
     Ok (EA "f" [EC "S" [EC "Z" []],EC "Cons" [EC "A" [],EC "Nil" []]],"")

     > runParser pExp "(f (S Z) (Cons A Nil)"
     Error <message>

   (Note: in the last one, the final closing paren is missing)
-}

{- 3 MARKS -}

{- 3.4.4 Parsing Rules.

   As described above, the concrete syntax for rules looks like:

      (plus (S x) y) = (S (plus x y))

   Using the 'pPat' and 'pExp' parsers you wrote above, write a
   'Parser' for equations. To be programmer friendly, you should be
   flexible about spaces, using the 'spaces' parser.

   Some test cases:

      > runParser pRule "(main) = Z"
      Ok (MkRule "main" [] (EC "Z" []),"")

      > runParser pRule "(myfunction x y) = (otherfunction y x)"
      Ok (MkRule "myfunction" [PV "x",PV "y"] (EA "otherfunction" [EV "y",EV "x"]),"")

      > runParser pRule "(myfunction x y) = (foo"
      Error <message>

      > runParser pRule "(myfunction x y) = foo)"
      Ok (MkRule "myfunction" [PV "x",PV "y"] (EV "foo"),")")

      > runParser pRule "(myfunction x y) = ()"
      Error <message>
-}

pRule :: Parser Rule
pRule = undefined

{- 3 MARKS -}

{- 3.4.5 Parsing lists of Rules, aka Programs.

   The final stage of parsing is a parser for lists of equations,
   separated by zero or more spaces. You should also allow for spaces
   at the beginning and end of the input too. -}

pProgram :: Parser Program
pProgram = undefined

{- 2 MARKS -}

{- Once you have implemented the parser and evaluator, you will be able
   to run GHOUL programs, like so:

      > runGHOUL plusProgram
      Ok (Ok (VC "S" [VC "S" [VC "S" [VC "S" [VC "Z" []]]]]))

   See also the file 'Ex3Main.hs', which contains a 'main' function
   for a standalone GHOUL interpreter. -}

{----------------------------------------------------------------------}
{- Part 5 : CHECKING                                                  -}
{----------------------------------------------------------------------}

{- The GHOUL interpreter that you wrote above tries its best with any
   'Program' that it is given, but there are some silly mistakes that
   programmers can make that can be relatively easily checked before
   execution.

   In this part, you will write two checks on GHOUL programs that
   check for mistakes:

     - Not having a 'main' function, or the main function taking
       arguments. (Question 3.5.0 below).

     - Using variables on the right-hand side of an equation that are
       not mentioned in the pattern on the left-hand side (Question
       3.5.1, below). For example:

          (plus Z x) = y

       This equation will always fail during execution, because there
       is no value for 'y'.

  The following function runs the two checks listed above. However,
  all of the functions that actually do the checking just immediately
  return successfully. It is your task to fill them in. -}

checkProgram :: Program -> ErrorOr ()
checkProgram prog =
  do hasMain prog
     scopeCheck prog

{- In the functions below, you can use the 'abortWithMessage' function
   defined above to report errors in the ErrorOr monad. -}

{- 3.5.0 Checking for a main function

   Write a function that checks a 'Program' for a rule for a function
   called 'main' that has no arguments. If it doesn't, then you should
   return a useful error message. It is up to you whether or not you
   regard having more than one 'main' as an error (the evaluator above
   will always take the first one).

   Examples:

     > hasMain []
     Error "Missing main function"

     > hasMain plusProgramAST
     Ok ()
-}

hasMain :: Program -> ErrorOr ()
hasMain prog = return () -- replace this

{- 2 MARKS -}


{- 3.5.1 Scope checking

   Write a function that checks each equation is "well-scoped". This
   means that all the variables mentioned on the right-hand side (in
   the 'Exp') are mentioned on the left-hand side (in the
   patterns). You may find it helpful to write a function that checks
   individual rules first, and then use 'mapM_' to check every
   rule in a program.

      > scopeCheck plusProgramAST
      OK ()

      > let badRule = MkRule "f" [PV "x"] (EA "plus" [EV "x", EV "y"])
      > scopeCheck [badRule]
      Error "Equation for f is not well scoped"
-}

scopeCheck :: Program -> ErrorOr ()
scopeCheck prog = return () -- replace this

{- 5 MARKS -}

{----------------------------------------------------------------------}
{- Part 6 : ADDITIONAL FEATURES                                       -}
{----------------------------------------------------------------------}

{- 3.6.0 I/O

   At the moment, GHOUL is a purely functional language. In this
   question, you'll implement a basic I/O feature for GHOUL.

   Extend GHOUL so that it has two special functions:

    - 'input' which takes no arguments and reads a line from the real
      input that represents a GHOUL value. It returns this value.

    - 'output' takes one argument and outputs it to the real
      output. It then returns the value 'Nothing'.

   Extend the implementation of GHOUL so that it has 'input' and
   'output' functions that act like this.

   You will need to:

     - Alter the definition of the 'Eval' type to allow for 'IO' to be
       performed.

     - Alter the 'Monad' instance of 'Eval' to do 'IO' effects as
       well.

     - Alter 'runEval', 'abortEvaluation', 'getCurrentProgram', and
       'liftError'.

     - Write a parser for 'Value's (will be similar to the parsers for
       'Pat's and 'Exp's)

     - Alter the definition of the 'eval' function so that it
       interprets 'input' and 'ouptut' as described above.

   Summarise the changes you make here, so that we know what you did: -}

{- 10 MARKS -}


{- 3.6.1 Built-in arithmetic

   As it stands, GHOUL is reasonably expressive but not very
   efficient. One source of inefficency is the fact that numbers are
   represented as sequences of 'S's followed by a 'Z'. This makes
   almost all operations on numbers take time linearly proportional to
   the size of the number, not to mention the wasted memory space.

   A more efficient approach would be to use Haskell's built in
   integer arithmetic for numbers. In this question, you should extend
   the GHOUL system so that it has support for values that are
   represented by Haskell values of type 'Int'.

   You will need to:

     1. Extend the type of 'Exp'ressions so that they can include
        'Int' constants. You'll need to extend 'ppExp' too.

     2. Extend the type of 'Value's so that values can be 'Int's as
        well as constructors applied to values. You'll also have to
        extend 'ppValue'.

     3. Extend expression evaluation so that special arithmetic
        functions are recognised and specially handled. At least you
        should implement 'add' that adds two 'Int' values, and 'eq'
        that compares two 'Int's for equality and returns 'True' if
        they are equal and 'False' otherwise.

     4. Extend the parser to allow for 'Int' constants (use the
        'number' parser defined below).

     5. (Optional) extend the pattern matcher so that it allows
        matching 'Int's as well a constructors.

   Summarise the changes you make here, so that we know what you did: -}

{- 10 MARKS -}

{----------------------------------------------------------------------}
{- APPENDIX : RUNNING GHOUL FROM THE COMMAND LINE                     -}
{----------------------------------------------------------------------}

{- See the file 'Ex3Main.hs' -}

{----------------------------------------------------------------------}
{- APPENDIX : ERROR PLUMBING                                          -}
{----------------------------------------------------------------------}

{- Here is the code that implements the Monad, Functor, Applicative and
   Alternative interfaces for the 'ErrorOr' type. -}

instance Monad ErrorOr where
  return x = Ok x
  Ok a    >>= f = f a
  Error s >>= _ = Error s

instance Functor ErrorOr where
  fmap f (Ok a)    = Ok (f a)
  fmap f (Error s) = Error s

instance Applicative ErrorOr where
  pure = return
  Ok f    <*> Ok a    = Ok (f a)
  Error s <*> _       = Error s
  _       <*> Error s = Error s

{----------------------------------------------------------------------}
{- APPENDIX : PARSER COMBINATORS                                      -}
{----------------------------------------------------------------------}

{- Here is the code for the parser combinators you should use to
   implement your GHOUL parser. You may want to consult this code to
   help you write your parser, but do not alter it. -}

instance Monad Parser where
  return x = MkParser (\s -> Ok (x,s))

  p >>= k =
    MkParser (\s -> case runParser p s of
                      Error msg  -> Error msg
                      Ok (a, s') -> runParser (k a) s')

instance Functor Parser where
  fmap f p =
    do x <- p
       return (f x)

instance Applicative Parser where
  pure = return
  pf <*> pa = do f <- pf; a <- pa; return (f a)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Ok (a,input1) -> Ok (a,input1)
                Error _       -> runParser p2 input)

failParse :: String -> Parser a
failParse msg = MkParser (\s -> abortWithMessage msg)

char :: Parser Char
char =
  MkParser
  (\input ->
      case input of
        []     -> Error "unexpected end of input was found"
        (c:cs) -> Ok (c, cs))

isChar :: Char -> Parser ()
isChar expected =
  do seen <- char
     if expected == seen then
       return ()
     else
       failParse ("Expecting " ++ show expected ++ ", got " ++ show seen)

satisfies :: String -> (Char -> Bool) -> Parser Char
satisfies p_description p = do
  c <- char
  if p c then return c
    else failParse ("Expecting " ++ p_description ++ ", got " ++ show c)

string :: String -> Parser ()
string expected =
  mapM_ isChar expected
  `orElse`
  failParse ("Expecting '" ++ expected ++ "'")

digit :: Parser Int
digit = do
  c <- char
  if isNumber c then
    return (digitToInt c)
  else
    failParse "Expecting a digit"

oneOrMore :: Parser a -> Parser [a]
oneOrMore p =
  do x  <- p
     xs <- zeroOrMore p
     return (x:xs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
  return []

number :: Parser Int
number =
  foldl (\l r -> l*10+r) 0 <$> oneOrMore digit
  `orElse`
  failParse "Expecting a positive number"

space :: Parser ()
space = do satisfies "a space character" isSpace
           return ()

spaces :: Parser ()
spaces = do zeroOrMore space
            return ()

identifier :: Parser String
identifier =
  do c  <- satisfies "alphabetic character" isAlpha
     cs <- zeroOrMore (satisfies "alphanumeric character" isAlphaNum)
     return (c:cs)
  `orElse`
  failParse "Expecting an identifier"

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p =
  do x  <- p
     xs <- zeroOrMore (do sep; p)
     return (x:xs)
  `orElse`
  return []
