{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where

import ABS.Runtime.CmdOpt
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar) -- futures
import ABS.Runtime.TQueue (TQueue) -- mailbox
import Data.IORef (IORef)
import Control.Monad.Trans.Cont (ContT)
import Data.Time.Clock (NominalDiffTime) -- for realtime
import Control.Distributed.Process (Process, NodeId (..), ProcessId, processNodeId)
import Network.Transport.TCP (encodeEndPointAddress)
import Data.Binary
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (when)
import qualified Data.Map.Strict as M (Map, empty, insert, lookup)

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
data Obj' contents = Obj' (IORef contents) !Cog' Word


instance Binary (Obj' a) where
  put o@(Obj' _ (Cog' _ _ pid _) counter) = do
    when (processNodeId pid == selfNode) $ 
      (unsafePerformIO (modifyMVar_ foreignStore (pure . M.insert (pid,counter) (AnyObj o)))) `seq` pure ()
    put pid *> put counter
  get = do
      pid <- get
      counter <- get
      if processNodeId pid == selfNode
        then let m = M.lookup (pid,counter) $ unsafePerformIO (readMVar foreignStore)
             in case m of
                -- or use the safer Data.Typeable.cast, or Data.Typeable.eqT
                Just (AnyObj o) ->  -- unsafePerformIO (print "found foreign") `seq`
                                    pure (unsafeCoerce o :: Obj' a)
                _ -> fail "foreign table lookup fail"
        else pure $ Obj' undefined (Cog' undefined undefined pid undefined) counter


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
data Cog' = Cog' (IORef SleepTable) (TQueue (ABS' ())) ProcessId (IORef Word)

instance Eq Cog' where
    (Cog' _ _ pid1 _) == (Cog' _ _ pid2 _) = pid1 == pid2

--instance Binary Cog' where
--  put (Cog' _ _ pid _) = put pid
--  get = do
--    pid <- get
--    pure (Cog' undefined undefined pid undefined)

type DC = NodeId

type SleepTable = [ (IO Bool     -- test function
                    ,ABS' ())  -- continuation
                  ]

type ABS' = ContT () Process


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

{-# NOINLINE selfNode #-}
selfNode:: NodeId
selfNode = NodeId $ encodeEndPointAddress (ip cmdOpt) (port cmdOpt) 0


{-# NOINLINE foreignStore #-}
foreignStore :: MVar (M.Map (ProcessId,Word) AnyObj)
foreignStore = unsafePerformIO $ newMVar M.empty
data AnyObj = forall a. AnyObj (Obj' a)