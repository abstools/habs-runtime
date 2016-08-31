{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where

import Control.Concurrent.MVar (MVar) -- futures
import ABS.Runtime.TQueue (TQueue) -- mailbox
import Data.IORef (IORef)
import Control.Monad.Trans.Cont (ContT)
import Data.Time.Clock (NominalDiffTime) -- for realtime
import Data.Ratio (Ratio)
import Unsafe.Coerce (unsafeCoerce)
-- | a future reference is a write-once locking var
--
-- write-once is not imposed by Haskell, but
-- we assume our translation respects this
-- NB: we deviate from ABS by not providing ordering of future-refs
type Fut a = MVar a

instance Show (MVar a) where
    show _ = "Fut"

-- | an object reference is a pair of
-- 1) a reference to its cog
-- 2) its attributes placed in a mutable variable=IORef
-- NB: we deviate from ABS by not providing ordering of object-refs
data Obj' contents = Obj' (IORef contents) !Cog DC

-- no need for Eq (Obj a). it is done by boilerplate generation of instance Eq I
-- instance Eq I where
--    I (Obj ioref1 _) == I (Obj ioref2 _) = ioref1 == ioref2
-- no need for Show (Obj a). show is done by the exposed interface-wrapper
-- instance Show I where
--    show _ = "I" 

-- | a COG reference is a mutex and a sleep table.
--
-- a process becomes active by acquiring&holding the COG's lock.
-- a process deactivates (by suspend,await) by releasing the lock.
data Cog = Cog (IORef SleepTable) (TQueue (ABS' ()))

instance Eq Cog where
    (Cog token1 _) == (Cog token2 _) = token1 == token2

type SleepTable = [ (IO Bool     -- test function
                    ,ABS' ())  -- continuation
                  ]

type ABS' = ContT () IO


-- | Subtyping-relation for ABS objects (as a multiparam typeclass)
class Sub' sub sup where
    -- | The upcasting method from a subtype to a supertype
    up' :: sub -> sup

-- self instance
instance Sub' a a where
    up' x = x

-- local variables in the statement are mutable references
type IORef' = IORef

data Null'

-- for realtime
type Time = NominalDiffTime

-- for simulating DC (extracted by code-generated src/ABS/DC.abs)

data Resourcetype = Speed
                  | Cores
                  | Bandwidth
                  | Memory
                  | Startupduration
                  | Shutdownduration
                  | PaymentInterval
                  | CostPerInterval
                  deriving (Eq, Show)

class DC' a where
        load :: Resourcetype -> Int -> Obj' a -> ABS' (Ratio Int)
        total :: Resourcetype -> Obj' a -> ABS' (Ratio Int)
        request__ :: Int -> Obj' a -> ABS' ()

data DC = forall a . DC' a => DC (Obj' a)

instance Show DC where
        show _ = "DC"

instance Eq DC where
        DC (Obj' ref1' _ _) == DC (Obj' ref2' _ _)
          = ref1' == unsafeCoerce ref2'

instance DC' Null' where
        load = undefined
        total = undefined
        request__ = undefined


instance DC' a => Sub' (Obj' a) DC where
        up' = DC