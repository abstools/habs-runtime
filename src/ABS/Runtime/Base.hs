{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where

import Control.Concurrent.MVar (MVar) -- futures
import ABS.Runtime.TQueue (TQueue) -- mailbox
import Data.IORef (IORef, newIORef)
import Control.Monad.Trans.Cont (ContT)
import System.Clock (TimeSpec) -- for realtime
import Data.Ratio (Ratio)
import Data.Dynamic (Dynamic)
import System.IO.Unsafe (unsafePerformIO)
import Data.Map (Map,empty)
import Control.Concurrent (ThreadId)
import System.Mem.Weak (Weak)
import qualified Data.Ratio (Ratio)
-- needed because of inclusion of generated code
import Prelude  as I' (fromIntegral, Bool, Int, String, IO, Eq (..),Ord(..), Show(..), undefined, error)
import qualified Unsafe.Coerce as I' (unsafeCoerce)

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
data Obj' contents = Obj' (IORef contents) !Cog DeploymentComponent

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

instance Sub' Int (Data.Ratio.Ratio Int) where
  up' = fromIntegral

-- local variables in the statement are mutable references
type IORef' = IORef

data Null'

-- realtime
type Time = TimeSpec

{-# NOINLINE apiStore' #-}
apiStore' :: IORef (Map String Dynamic)
apiStore' = unsafePerformIO (newIORef empty)

-- for simulating DC (extracted by code-generated src/ABS/DCInterface.abs)
type Unit = ()
type Rat = Data.Ratio.Ratio Int


-------------- GEN
data InfRat = InfRat
            | Fin !Rat
            deriving (I'.Eq, I'.Ord, I'.Show)
finvalue (Fin a) = a
finvalue _ = I'.error "Data constructor does not have accessor finvalue"


data Resourcetype = Speed
                  | Cores
                  | Bandwidth
                  | Memory
                  | Startupduration
                  | Shutdownduration
                  | PaymentInterval
                  | CostPerInterval
                  deriving (I'.Eq, I'.Ord, I'.Show)


class DeploymentComponent' a where
        
        load :: Resourcetype -> Int -> Obj' a -> ABS' Rat
        
        
        total :: Resourcetype -> Obj' a -> ABS' InfRat
        
        
        transfer ::
                 DeploymentComponent -> Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        
        decrementResources :: Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        
        incrementResources :: Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        
        getName :: Obj' a -> ABS' String
        
        
        getCreationTime :: Obj' a -> ABS' Time
        
        
        getStartupDuration :: Obj' a -> ABS' Rat
        
        
        getShutdownDuration :: Obj' a -> ABS' Rat
        
        
        getPaymentInterval :: Obj' a -> ABS' Int
        
        
        getCostPerInterval :: Obj' a -> ABS' Rat
        
        
        getNumberOfCores :: Obj' a -> ABS' Rat
        
        
        acquire :: Obj' a -> ABS' Bool
        
        
        release :: Obj' a -> ABS' Bool
        
        
        shutdown_ :: Obj' a -> ABS' Bool

        
        request' :: Int -> Obj' a -> ABS' Unit

        register' :: Weak ThreadId -> Obj' a -> ABS' ()

data DeploymentComponent = forall a . DeploymentComponent' a =>
                             DeploymentComponent (Obj' a)

instance I'.Show DeploymentComponent where
        show _ = "DeploymentComponent"

instance I'.Eq DeploymentComponent where
        DeploymentComponent (Obj' ref1' _ _) ==
          DeploymentComponent (Obj' ref2' _ _) = ref1' == I'.unsafeCoerce ref2'

instance DeploymentComponent' Null' where
        load = I'.undefined
        total = I'.undefined
        transfer = I'.undefined
        decrementResources = I'.undefined
        incrementResources = I'.undefined
        getName = I'.undefined
        getCreationTime = I'.undefined
        getStartupDuration = I'.undefined
        getShutdownDuration = I'.undefined
        getPaymentInterval = I'.undefined
        getCostPerInterval = I'.undefined
        getNumberOfCores = I'.undefined
        acquire = I'.undefined
        release = I'.undefined
        shutdown_ = I'.undefined
        request' = I'.undefined

instance DeploymentComponent' a => Sub' (Obj' a) DeploymentComponent where 
            up' = DeploymentComponent