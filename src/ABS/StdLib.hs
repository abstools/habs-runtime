{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module ABS.StdLib
    (
     -- * ABS builtin types

     -- | The ABS standard datatypes, most coming from standard Haskell, except Fut coming from @habs-runtime@ package
     Int, Rat, Prelude.Bool (..), Unit, List, Prelude.String, -- Fut
     -- * Operations on numbers
     (Prelude.+), (Prelude.-), (Prelude.*), (%), Prelude.abs, pow,
     -- * Operations on rationals
     Data.Ratio.numerator, Data.Ratio.denominator,
     -- * Rational division. Takes any number but always returns a Rat.
     (Prelude./),
     -- * Rat to Int conversion
     Prelude.truncate,
     -- * Boolean Operations 
     (Prelude.||), (Prelude.&&), (Prelude.==), Prelude.not,
     -- * Ordering operations
     (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), Prelude.min, Prelude.max, Prelude.minimum, Prelude.maximum,
     -- * Built-in Pairs and Triples and their functions
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd, fmap'Pair, fmap'Triple,
     -- * Maybe, Either datatypes and their functions
     Prelude.Maybe (..), fromJust, isJust, fmap'Maybe,
     Prelude.Either (..), left, right, isLeft, isRight, fmap'Either,
     -- * Functions for "List" datastructures
     list, Prelude.tail, Prelude.head, length, isEmpty, nth, concatenate, appendright, without, Prelude.repeat, Prelude.reverse, copy, fmap'List,
     -- * The ABS Map datatype and its functions
     M.Map, map, _emptyMap, put, insert, lookup, lookupMaybe, lookupUnsafe, lookupDefault, removeKey, keys, values,
     -- * The ABS Set datatype and its functions
     S.Set, set, _emptySet, emptySet, S.size, contains, S.union, S.intersection, S.difference, insertElement, remove, take, hasNext, next,
     -- * Printing to Strings and to standard-output
     toString, intToString, substr, strlen,
     -- * For Realtime
     Time, timeValue, timeDifference
    ) where

import qualified Prelude as Prelude
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Data.List (length)
import qualified Data.Ratio
import Data.Either (isLeft, isRight)
import Data.Bifunctor (bimap)
import System.Clock (TimeSpec, diffTimeSpec, toNanoSecs) -- for realtime

-- | Modulo operation. Takes two 'Rat's and returns an integer. 
--
-- Truncated towards 0, so it is Haskell's 'rem'.
{-# INLINE (%) #-}
(%) :: Rat -> Rat -> Int
x % y = let res = x Prelude./ y
        in Data.Ratio.numerator res `Prelude.rem` Data.Ratio.denominator res

-- | Raising a number to a non-negative integer power
--
-- Note: deviation, abstools defines pow also for negative integral powers, but then
-- the result always will be a Rat: int/rat conversion issue
-- 
-- Another way is to always return a Rat with (^^) and then to explicitly truncate if you want Int.
{-# INLINE pow #-}
pow :: Prelude.Num a => a -> Int -> a
pow = (Prelude.^)

type Unit = ()

type Int = Prelude.Int

type Bool = Prelude.Bool

type Rat = Data.Ratio.Ratio Prelude.Int

-------- LISTS--------------
----------------------------

-- | An ABS synonym to underneath Haskell lists. It is the same as defining in ABS:
--
-- > data List a = Nil | Cons a (List a)
type List = [] 

-- | Returns the element of the list positioned at the given index.
{-# INLINE nth #-}
nth :: List a -> Int -> a
nth = (Prelude.!!)

-- | Checks if the list is empty.
{-# INLINE isEmpty #-}
isEmpty :: List a -> Bool
isEmpty = Prelude.null

-- | Replicating an element 'n' times, forming a list of length n.
{-# INLINE copy #-}
copy :: a -> Int -> List a
copy = Prelude.flip Prelude.replicate

-- | Removes all occurences of an element from a list
{-# INLINE without #-}
without :: Prelude.Eq t => [t] -> t -> [t]
without [] _ = []
without (x:xs) a | x Prelude.== a = without xs a 
                 | Prelude.otherwise = x : without xs a

{-# INLINE concatenate #-}
concatenate :: [a] -> [a] -> [a]
concatenate = (Prelude.++)

{-# INLINE appendright #-}
appendright :: [a] -> a -> [a]
appendright l p = l Prelude.++ [p]

-- | dummy function for ABS n-ary constructors
{-# INLINE list #-}
list :: [a] -> [a]
list = Prelude.id

-------- MAPS---------------
----------------------------

{-# INLINE put #-}
put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

{-# INLINE insert #-}
insert :: Prelude.Ord k => M.Map k v -> (k,v) -> M.Map k v
insert m (k,v) = M.insert k v m

{-# INLINE lookup #-}
lookup :: Prelude.Ord k => M.Map k v -> k -> Prelude.Maybe v
lookup = Prelude.flip M.lookup

{-# DEPRECATED lookupMaybe "Use lookup instead" #-}
{-# INLINE lookupMaybe #-}
-- | Synonym to lookup
lookupMaybe :: Prelude.Ord k => M.Map k v -> k -> Prelude.Maybe v
lookupMaybe = lookup

{-# INLINE lookupUnsafe #-}
lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k


-- | Returns the value associated with key 'k' in map 'ms', or the value 'd'
-- if 'k' has no entry in 'ms'.
{-# INLINE lookupDefault #-}
lookupDefault :: Prelude.Ord k => M.Map k a -> k -> a -> a
lookupDefault ms k d = M.findWithDefault d k ms

{-# INLINE removeKey #-}
removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

-- | Constructing maps from an association list.
{-# INLINE map #-}
map :: Prelude.Ord k => List (Pair k a) -> M.Map k a
map = M.fromList

{-# INLINE values #-}
values :: M.Map k a -> (List a)
values = M.elems

-- | Constructor of empty Maps, 'EmptyMap' in ABS
{-# INLINE _emptyMap #-}
_emptyMap :: M.Map k a
_emptyMap = M.empty


{-# INLINE keys #-}
keys :: M.Map k a -> S.Set k
keys = M.keysSet

-------- SETS---------------
----------------------------

{-# INLINE set #-}
set :: Prelude.Ord a => List a -> S.Set a
set = S.fromList

{-# INLINE contains #-}
contains :: Prelude.Ord a => S.Set a -> a -> Bool
contains = Prelude.flip S.member

{-# INLINE emptySet #-}
emptySet :: S.Set a -> Bool
emptySet = S.null

{-# INLINE _emptySet #-}
_emptySet :: S.Set a
_emptySet = S.empty

{-# INLINE insertElement #-}
insertElement :: Prelude.Ord a => S.Set a -> a -> S.Set a
insertElement = Prelude.flip S.insert

{-# INLINE remove #-}
remove :: Prelude.Ord a => S.Set a -> a -> S.Set a
remove = Prelude.flip S.delete

-- | Returns one (arbitrary) element from a set.
-- To iterate over a set, take one element and remove it from the set.
-- Repeat until set is empty.
{-# INLINE take #-}
take :: S.Set a -> a
take = S.findMin

{-# INLINE hasNext #-}
hasNext :: S.Set a -> Bool
hasNext s = Prelude.not (S.null s)

{-# INLINE next #-}
-- | NB: partial function, combine it with 'hasNext'.
next :: S.Set a -> (S.Set a, a)
next s = case S.minView s of
    Prelude.Just (v,s') -> (s',v)
    Prelude.Nothing -> Prelude.error "next() called on empty set"
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
left :: Prelude.Either a b -> a
left (Prelude.Left a ) = a
left _ = Prelude.error "not a left-Either"

-- | Deconstructs _unsafely_  the right part of an Either
right :: Prelude.Either a b -> b
right (Prelude.Right a) = a
right _ = Prelude.error "not a right-Either"

-------- STRINGS------------
----------------------------

-- trick to overload the (+) for concatenating strings
instance Prelude.Num [Prelude.Char] where
  x + y = x Prelude.++ y
  (-) = Prelude.undefined
  (*) = Prelude.undefined
  abs = Prelude.undefined
  signum = Prelude.undefined
  fromInteger = Prelude.undefined
  
{-# INLINE toString #-}
toString :: Prelude.Show a => a -> Prelude.String
toString = Prelude.show

-- | Returns a string with the base-10 textual representation of 'n'.
-- Note: Will work the same as toString. Just a carry-over from the other frontend.
{-# INLINE intToString #-}
intToString :: Int -> Prelude.String
intToString = Prelude.show

-- | Returns a substring of string str of the given length starting from start (inclusive)
-- Where the first character has index 0
-- 
-- Example:
--    substr("abcde",1,3) => "bcd"
{-# INLINE substr #-}
substr :: Prelude.String -> Int -> Int -> Prelude.String
substr str d len = Prelude.take len (Prelude.drop d str)

{-# INLINE strlen #-}
strlen :: Prelude.String -> Int
strlen = Prelude.length


-- Realtime ABS

type Time = TimeSpec

{-# INLINE timeDifference #-}
timeDifference :: Time -> Time -> Rat
timeDifference x y = Prelude.fromIntegral (toNanoSecs (diffTimeSpec x y)) Prelude./ 1000000

{-# INLINE timeValue #-}
timeValue :: Time -> Rat
timeValue = (Prelude./ 1000000) Prelude.. Prelude.fromIntegral Prelude.. toNanoSecs


-- ALIASES for genifunctors subtyping
{-# INLINE fmap'List #-}
fmap'List :: (a->b) -> [a] -> [b]
fmap'List = Prelude.fmap

{-# INLINE fmap'Maybe #-}
fmap'Maybe :: (a->b) -> Prelude.Maybe a -> Prelude.Maybe b
fmap'Maybe = Prelude.fmap


{-# INLINE fmap'Either #-}
fmap'Either :: (a->c) -> (b->d) -> Prelude.Either a b -> Prelude.Either c d
fmap'Either = bimap


{-# INLINE fmap'Pair #-}
fmap'Pair :: (a->c) -> (b->d) -> Pair a b -> Pair c d
fmap'Pair = bimap

fmap'Triple :: (a ->a1) -> (b -> b1) -> (c -> c1) -> (a,b,c) -> (a1,b1,c1)
fmap'Triple f g h ~(a,b,c) = (f a, g b, h c) 




