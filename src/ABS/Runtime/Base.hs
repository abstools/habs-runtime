{-# LANGUAGE MultiParamTypeClasses #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where


import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Control.Monad.Trans.Cont (ContT)

-- | a future reference is a write-once locking var
--
-- write-once is not imposed by Haskell, but
-- we assume our translation respects this
-- NB: we deviate from ABS by not provigin ordering of future-refs
newtype Fut a = Fut {
      fromFut :: MVar a
    } deriving Eq

instance Show (Fut a) where
    show _ = "Fut"

-- | an object reference is a pair of
-- 1) a reference to its cog
-- 2) its attributes placed in a mutable variable=IORef
-- NB: we deviate from ABS by not providing ordering of object-refs
data Obj contents = Obj Cog (IORef contents)

-- | a COG reference is a mutex and a sleep table.
--
-- a process becomes active by acquiring&holding the COG's lock.
-- a process deactivates (by suspend,await) by releasing the lock.
data Cog = Cog (MVar ()) (IORef SleepTable)

instance Eq (Obj a) where
    (Obj _ ioref1) == (Obj _ ioref2) = ioref1 == ioref2

instance Eq Cog where
    (Cog token1 _) == (Cog token2 _) = token1 == token2

type SleepTable = [ (IO Bool     -- test function
                    ,() -> ABS ())  -- continuation
                  ]

type ABS a = ContT () IO a


-- | Subtyping-relation for ABS objects (as a multiparam typeclass)
class Sub sub sup where
    -- | The upcasting method from a subtype to a supertype
    up :: sub -> sup


-- TODO: define also exceptions here later




