{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, FlexibleInstances #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where

import Control.Concurrent.MVar (MVar) -- futures
import ABS.Runtime.TQueue (TQueue) -- mailbox
import Data.IORef (IORef, newIORef)
import Control.Monad.Trans.Cont (ContT)
import Data.Dynamic (Dynamic)
import System.IO.Unsafe (unsafePerformIO)
import Data.Map (Map,empty)
import qualified Data.Ratio (Ratio)

-- | Uninhabited. It is used to initially-type the null static object and the this in (ABS' this) of main block.
data Null'

-- | Our ABS stateful code is actually an IO-enabled semi-coroutine.
type ABS' = ContT () IO

-- | Local variables in stateful code are actually GHC's heap mutable references.
--
-- Note: this type synonym is redundant. It is only used for haddock documentation and not actual code-generation.
type IORef' = IORef

-- | A future reference is a write-once locking var.
--
-- Write-once is not imposed by Haskell, but
-- we assume our translation respects this
--
-- NB: we deviate from ABS by not providing ordering of future-refs
type Fut a = MVar a

instance Show (MVar a) where
    show _ = "Fut"


-- | A COG reference is a mutex and a sleep table.
--
-- A process becomes active by acquiring&holding the COG's lock.
-- A process deactivates (by suspend,await) by releasing the lock.
data Cog = Cog (IORef SleepTable) (TQueue (ABS' ()))

instance Eq Cog where
    (Cog token1 _) == (Cog token2 _) = token1 == token2

-- | The sleep table is a naive assoc-list of boolean conditions to their suspend continuations.
type SleepTable = [ (IO Bool     -- test function
                    ,ABS' ())  -- continuation
                  ]

-- | an object reference is a pair of
-- 1) a reference to its cog
-- 2) its attributes placed in a mutable variable=IORef
--
-- NB: we deviate from ABS by not providing ordering of object-refs
data Obj' contents = Obj' (IORef contents) !Cog 

-- no need for Eq (Obj a). it is done by boilerplate generation of instance Eq I
-- instance Eq I where
--    I (Obj ioref1 _) == I (Obj ioref2 _) = ioref1 == ioref2
-- no need for Show (Obj a). show is done by the exposed interface-wrapper
-- instance Show I where
--    show _ = "I" 

-- | Subtyping-relation for ABS objects (as a multiparam typeclass).
class Sub' sub sup where
    -- | The upcasting method from a subtype to a supertype
    up' :: sub -> sup

instance Sub' Int (Data.Ratio.Ratio Int) where
  up' = fromIntegral


{-# NOINLINE apiStore' #-}
-- | A dynamic key-value store that stores the HTTPCallable names of objects to their heap reference (IORef). 
--
-- This technique effectively disables the garbage-collection for any of the exposed objects, since the reference
-- is stored as a 'Dynamic' and not as a weak pointer.
apiStore' :: IORef (Map String Dynamic)
apiStore' = unsafePerformIO (newIORef empty)