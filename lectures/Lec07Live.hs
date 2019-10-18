{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lec07Live where

import Prelude hiding (Left, Right, Maybe (..), Semigroup (..))
import Data.Char

{-     LECTURE 07 : MODELLING WITH DATATYPES  -}

-- one :: Int
-- one = if True then 1 else "one"

{-   Part I : TYPE SYNONYMS -}

type Metres = Double

distanceToTheMoon :: Metres
                  -- Double
distanceToTheMoon = 384402000

add :: Double -> Double -> Double
add x y = x + y

type Seconds = Double

secondsInAMinute :: Seconds
secondsInAMinute = 60

type Position = (Int,Int)

origin :: Position
origin = (0,0)

type Transformation = Position -> Position

goUp :: Transformation
     -- Position -> Position
     -- (Int,Int) -> (Int,Int)
goUp (x,y) = (x,y+1)

f :: (Position -> Int) -> Int
f g = g origin

type Pair a b = (a,b)

type Position' = Pair Int Int
              -- (Int,Int)


{-    Part 2 : DATA TYPE DEFINITIONS -}

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving Show

move :: Direction -> Transformation
     -- Direction -> (Position -> Position)
     -- Direction -> ((Int,Int) -> (Int,Int))
move Up    (x,y) = (x,y+1)
move Down  (x,y) = (x,y-1)
move Left  (x,y) = (x-1,y)
move Right (x,y) = (x+1,y)

data GridDirection
  = Vertical Int
  | Horizontal Int
  deriving Show

toGridD :: Direction -> GridDirection
toGridD Up    = Vertical 1
toGridD Down  = Vertical (-1)
toGridD Left  = Horizontal (-1)
toGridD Right = Horizontal 1

data Maybe a
  = Nothing
  | Just a
  deriving Show

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []           = Nothing
search k ((k',v):kvs) = if k == k' then Just v else search k kvs

--   V get(Object o)

kv :: [(Int,Maybe String)]
kv = [(1,Just "one"), (-1, Nothing)]

-- Optional<Integer>

{-  1. Student name (required)
    2. At least one of:
       - registration number
       - DS username

    public class Student {
        @Nonnull
        private String name;

        // at least one of these is nonnull
        private String regNum;
        private String dsUsername;
    }
-}

data Student = MkStudent { name    :: String
                         , details :: StudentDetails
                         } deriving Show

data StudentDetails
  = RegNumber  String
  | DSUsername String
  | RegAndDS   String String
  deriving Show



data Kilograms = MkKilograms Double
  deriving Show

doubleMass :: Kilograms -> Kilograms
doubleMass (MkKilograms x) = MkKilograms (x*2)

newtype Feet = MkFeet Double
  deriving Show


{-   Part III : TYPE CLASSES -}

{-   Object.equal(Object o)
-}

data CompassDirection
  = North
  | South
  | East
  | West

instance Eq CompassDirection where
  North == North = True
  North == South = True
  South == North = True
  South == South = True
  East  == East  = True
  West  == West  = True
  _     == _     = False

instance Show CompassDirection where
  show North = "N"
  show South = "S"
  show East  = "E"
  show West  = "W"

