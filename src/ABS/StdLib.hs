{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, OverloadedStrings, TemplateHaskell, PartialTypeSignatures, ExplicitForAll #-}

module ABS.StdLib
    (
     -- * ABS builtin types

     -- | The ABS standard datatypes, most coming from standard Haskell, except Fut coming from @habs-runtime@ package
     Int, Rat, Bool (..), Unit, List, String, -- Fut
     -- * Operations on numbers
     (+), (-), (*), (%), abs, pow,
     -- * Operations on rationals
     numerator, denominator,
     -- * Rational division. Takes any number but always returns a Rat.
     (/),
     -- * Rat to Int conversion
     truncate,
     -- * Boolean Operations 
     (||), (&&), (==), not,
     -- * Ordering operations
     (<), (<=), (>=), (>), min, max, minimum, maximum,
     -- * Built-in Pairs and Triples and their functions
     Pair, fst, snd, Triple, fstT, sndT, trd, fmap'Pair, fmap'Triple,
     -- * Maybe, Either datatypes and their functions
     Maybe (..), fromJust, isJust, fmap'Maybe,
     Either (..), left, right, isLeft, isRight, fmap'Either,
     -- * Functions for "List" datastructures
     list, tail, head, length, isEmpty, nth, concatenate, appendright, without, repeat, reverse, copy, fmap'List,
     -- * The ABS Map datatype and its functions
     Map, map, put, insert, lookup, lookupMaybe, lookupUnsafe, lookupDefault, removeKey, keys, values,
     -- * The ABS Set datatype and its functions
     Set, set, emptySet, size, contains, union, intersection, difference, insertElement, remove, take, hasNext, next,
     -- * Printing to Strings and to standard-output
     toString, intToString, substr, strlen,
     -- * For Realtime
     Time, timeValue, timeDifference
    ) where

import ABS.Runtime.Base (Time)
import Prelude (Int, Bool (..), String, (+), (-), (*), abs, (/), (||), truncate, not,  (&&), (==), (<), (<=), (>=), (>), min, max, minimum, maximum, fst, snd, Maybe (..), Either (..), tail, head, repeat, reverse)
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromJust, isJust)
import Data.List (length)
import Data.Either (isLeft, isRight)
import Data.Bifunctor (bimap)
import System.Clock (diffTimeSpec, toNanoSecs) -- for realtime
-- qualified importing for not re-exported identifiers
import qualified Prelude as I'
import qualified Data.Ratio as I' (Ratio)
-- for rest api
import qualified Data.Aeson as J (ToJSON (..), Value(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Generics.Genifunctors as I' (genFmap)

-- | Modulo operation. Takes two 'Rat's and returns an integer. 
--
-- Truncated towards 0, so it is Haskell's 'rem'.
{-# INLINE (%) #-}
(%) :: Rat -> Rat -> Int
x % y = let res = x / y
        in numerator res `I'.rem` denominator res

-- | Raising a number to a non-negative integer power
--
-- Note: deviation, abstools defines pow also for negative integral powers, but then
-- the result always will be a Rat: int/rat conversion issue
-- 
-- Another way is to always return a Rat with (^^) and then to explicitly truncate if you want Int.
{-# INLINE pow #-}
pow :: I'.Num a => a -> Int -> a
pow = (I'.^)

type Unit = ()

--type Int = I'.Int
--type Bool = I'.Bool

type Rat = I'.Ratio Int

-------- LISTS--------------
----------------------------

-- | An ABS synonym to underneath Haskell lists. It is the same as defining in ABS:
--
-- > data List a = Nil | Cons a (List a)
type List = [] 

-- | Returns the element of the list positioned at the given index.
{-# INLINE nth #-}
nth :: List a -> Int -> a
nth = (I'.!!)

-- | Checks if the list is empty.
{-# INLINE isEmpty #-}
isEmpty :: List a -> Bool
isEmpty = I'.null

-- | Replicating an element 'n' times, forming a list of length n.
{-# INLINE copy #-}
copy :: a -> Int -> List a
copy = I'.flip I'.replicate

-- | Removes all occurences of an element from a list
{-# INLINE without #-}
without :: I'.Eq t => [t] -> t -> [t]
without [] _ = []
without (x:xs) a | x == a = without xs a 
                 | I'.otherwise = x : without xs a

{-# INLINE concatenate #-}
concatenate :: [a] -> [a] -> [a]
concatenate = (I'.++)

{-# INLINE appendright #-}
appendright :: [a] -> a -> [a]
appendright l p = l I'.++ [p]

-- | dummy function for ABS n-ary constructors
{-# INLINE list #-}
list :: [a] -> [a]
list = I'.id


-------- TUPLES-------------
----------------------------

type Pair a b = (a,b)

type Triple a b c = (a,b,c)
fstT :: Triple a b c -> a
fstT (a,_,_) = a
sndT :: Triple a b c -> b
sndT (_,b,_) = b
trd :: Triple a b c -> c
trd (_,_,c) = c

-- | Deconstructs _unsafely_ the left part of an Either
left :: Either a b -> a
left (Left a ) = a
left _ = I'.error "not a left-Either"

-- | Deconstructs _unsafely_  the right part of an Either
right :: Either a b -> b
right (Right a) = a
right _ = I'.error "not a right-Either"

-------- STRINGS------------
----------------------------

-- trick to overload the (+) for concatenating strings
instance I'.Num [I'.Char] where
  x + y = x I'.++ y
  (-) = I'.undefined
  (*) = I'.undefined
  abs = I'.undefined
  signum = I'.undefined
  fromInteger = I'.undefined
  
{-# INLINE toString #-}
toString :: I'.Show a => a -> String
toString = I'.show

-- | Returns a string with the base-10 textual representation of 'n'.
-- Note: Will work the same as toString. Just a carry-over from the other frontend.
{-# INLINE intToString #-}
intToString :: Int -> String
intToString = I'.show

-- | Returns a substring of string str of the given length starting from start (inclusive)
-- Where the first character has index 0
-- 
-- Example:
--    substr("abcde",1,3) => "bcd"
{-# INLINE substr #-}
substr :: String -> Int -> Int -> String
substr str d len = I'.take len (I'.drop d str)

{-# INLINE strlen #-}
strlen :: String -> Int
strlen = I'.length


-- Realtime ABS

{-# INLINE timeDifference #-}
timeDifference :: Time -> Time -> Rat
timeDifference x y = I'.fromIntegral (toNanoSecs (diffTimeSpec x y)) / 1000000

{-# INLINE timeValue #-}
timeValue :: Time -> Rat
timeValue = (/ 1000000) I'.. I'.fromIntegral I'.. toNanoSecs


-- ALIASES for genifunctors subtyping
{-# INLINE fmap'List #-}
fmap'List :: (a->b) -> [a] -> [b]
fmap'List = I'.fmap

{-# INLINE fmap'Maybe #-}
fmap'Maybe :: (a->b) -> Maybe a -> Maybe b
fmap'Maybe = I'.fmap


{-# INLINE fmap'Either #-}
fmap'Either :: (a->c) -> (b->d) -> Either a b -> Either c d
fmap'Either = bimap


{-# INLINE fmap'Pair #-}
fmap'Pair :: (a->c) -> (b->d) -> Pair a b -> Pair c d
fmap'Pair = bimap

fmap'Triple :: (a ->a1) -> (b -> b1) -> (c -> c1) -> (a,b,c) -> (a1,b1,c1)
fmap'Triple f g h ~(a,b,c) = (f a, g b, h c) 



-- For REST API (not proper JSON output, only specific to the frh case study)
instance {-# OVERLAPS #-} J.ToJSON (List (Pair Time (List (Pair String Rat)))) where
  toJSON l = J.Object (H.singleton "result" (J.String (show' l)))


class Show' a where
  show' :: a -> T.Text


instance Show' a => Show' (List a) where
  show' [] = "list[]"
  show' ls = "list[" `T.append` (T.intercalate "," (I'.fmap show' ls)) `T.append` "]"



instance (Show' a, Show' b) => Show' (Pair a b) where
  show' (a,b) = "Pair(" `T.append` show' a `T.append` "," `T.append` show' b `T.append` ")"



instance Show' Rat where
  show' r = T.pack (I'.show (Data.Ratio.numerator r)) `T.append` "/" `T.append` T.pack (I'.show (Data.Ratio.denominator r))



instance {-# OVERLAPS #-} Show' String where
  show' s = "\"" `T.append` T.pack s `T.append` "\""



instance Show' Time where
  show' t = "Time(" `T.append` T.pack ( I'.show (toNanoSecs t `I'.quot` 1000000)) `T.append` ")" -- ms


-- Generated slow Sets and Maps fro FRH case study
--------------------------------------------------

data Set a = EmptySet
           | Insert !a !(Set a)
           deriving (I'.Eq, I'.Ord, I'.Show)


data Map a b = EmptyMap
             | InsertAssoc !(Pair a b) !(Map a b)
             deriving (I'.Eq, I'.Ord, I'.Show)

I'.return []
fmap'Map = $( I'.genFmap ''Map )
fmap'Set = $( I'.genFmap ''Set )

set :: forall a . _ => List a -> Set a

set l
  = case l of
        [] -> (EmptySet)
        (x : xs) -> (insertElement (set xs) x)

contains :: forall a . _ => Set a -> a -> Bool

contains ss e
  = case ss of
        EmptySet -> (False)
        Insert e' _ | e' == e -> (True)
        Insert x xs -> if (x > e) then (False) else (contains xs e)

emptySet :: forall a . _ => Set a -> Bool

emptySet xs = (xs == (EmptySet))

size :: forall a . _ => Set a -> Int

size xs
  = case xs of
        EmptySet -> 0
        Insert s ss -> (1 + (size ss))

union :: forall a . _ => Set a -> Set a -> Set a

union set1 set2
  = case set1 of
        EmptySet -> set2
        Insert e1 ss1 -> case set2 of
                             EmptySet -> set1
                             Insert e1 ss2 -> (Insert e1 (union ss1 ss2))
                             Insert e2 ss2 -> if (e1 < e2) then (Insert e1 (union ss1 set2))
                                                else (Insert e2 (union set1 ss2))

intersection :: forall a . _ => Set a -> Set a -> Set a

intersection set1 set2
  = case set1 of
        EmptySet -> (EmptySet)
        Insert e1 ss1 -> case set2 of
                             EmptySet -> (EmptySet)
                             Insert e1 ss2 -> (Insert e1 (intersection ss1 ss2))
                             Insert e2 ss2 -> if (e1 < e2) then (intersection ss1 set2) else
                                                (intersection set1 ss2)

difference :: forall a . _ => Set a -> Set a -> Set a

difference set1 set2
  = case set1 of
        EmptySet -> (EmptySet)
        Insert e1 ss1 -> case set2 of
                             EmptySet -> set1
                             Insert e1 ss2 -> (difference ss1 ss2)
                             Insert e2 ss2 -> if (e1 < e2) then
                                                (Insert e1 (difference ss1 set2)) else
                                                (difference set1 ss2)

insertElement :: forall a . _ => Set a -> a -> Set a

insertElement xs e
  = case xs of
        EmptySet -> (Insert e (EmptySet))
        Insert e' _ | e' == e -> xs
        Insert x ss -> if (e < x) then (Insert e xs) else
                         (Insert x (insertElement ss e))

remove :: forall a . _ => Set a -> a -> Set a

remove xs e
  = case xs of
        EmptySet -> (EmptySet)
        Insert e' ss | e' == e -> ss
        Insert x ss -> if (e < x) then xs else (Insert x (remove ss e))

take :: forall a . _ => Set a -> a

take ss
  = case ss of
        Insert e _ -> e

hasNext :: forall a . _ => Set a -> Bool

hasNext s = (not (emptySet s))

next :: forall a . _ => Set a -> Pair (Set a) a

next s
  = case s of
        Insert e set2 -> ((set2, e))

map :: forall a b . _ => List (Pair a b) -> Map a b

map l
  = case l of
        [] -> (EmptyMap)
        (hd : tl) -> (InsertAssoc hd (map tl))

removeKey :: forall a b . _ => Map a b -> a -> Map a b

removeKey map key
  = case map of
        EmptyMap -> map
        InsertAssoc (key', _) m | key' == key -> m
        InsertAssoc pair tail -> (InsertAssoc pair (removeKey tail key))

values :: forall a b . _ => Map a b -> List b

values map
  = case map of
        EmptyMap -> []
        InsertAssoc (_, elem) tail -> (elem : (values tail))

keys :: forall a b . _ => Map a b -> Set a

keys map
  = case map of
        EmptyMap -> (EmptySet)
        InsertAssoc (a, _) tail -> (insertElement (keys tail) a)

lookup :: forall a b . _ => Map a b -> a -> Maybe b

lookup ms k
  = case ms of
        InsertAssoc (k', y) _ | k' == k -> (Just y)
        InsertAssoc _ tm -> (lookup tm k)
        EmptyMap -> (Nothing)

lookupMaybe :: forall a b . _ => Map a b -> a -> Maybe b

lookupMaybe ms k = (lookup ms k)

lookupUnsafe :: forall a b . _ => Map a b -> a -> b

lookupUnsafe ms k = (fromJust (lookup ms k))

lookupDefault :: forall a b . _ => Map a b -> a -> b -> b

lookupDefault ms k d
  = case ms of
        InsertAssoc (k', y) _ | k' == k -> y
        InsertAssoc _ tm -> (lookupDefault tm k d)
        EmptyMap -> d

insert :: forall a b . _ => Map a b -> Pair a b -> Map a b

insert map p = (InsertAssoc p map)

put :: forall a b . _ => Map a b -> a -> b -> Map a b

put ms k v
  = case ms of
        EmptyMap -> (InsertAssoc ((k, v)) (EmptyMap))
        InsertAssoc (k', _) ts | k' == k -> (InsertAssoc ((k, v)) ts)
        InsertAssoc p ts -> (InsertAssoc p (put ts k v))
