{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, FlexibleInstances, ExistentialQuantification #-}
-- | All the types and datastructures used in the ABS-Haskell runtime
module ABS.Runtime.Base where

import ABS.Runtime.CmdOpt
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar) -- futures
import Control.Concurrent.STM.TQueue (TQueue) -- mailbox
import Data.IORef (IORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT)
import Data.Time.Clock (NominalDiffTime) -- for realtime
import Control.Distributed.Process (Process, NodeId (..), ProcessId, processNodeId, expect, send)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Node (forkProcess)
import Network.Transport.TCP (encodeEndPointAddress)
import Data.Binary
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (when, forever)
import qualified Data.Map.Strict as M (Map, empty, insert, lookup)

-- | a future reference is a write-once locking var
--
-- write-once is not imposed by Haskell, but
-- we assume our translation respects this
-- NB: we deviate from ABS by not providing ordering of future-refs
--type Fut a = MVar a
data Fut a = LocalFut' ProcessId Word (MVar a)
           | RemoteFut' ProcessId Word ProcessId

instance Show (Fut a) where
    show _ = "Fut"

instance Eq (Fut a) where
  LocalFut' _ _ mvar1 == LocalFut' _ _ mvar2 = mvar1 == mvar2
  RemoteFut' _ _ forwarderPid1 == RemoteFut' _ _ forwarderPid2 = forwarderPid1 == forwarderPid2
  LocalFut' creatorPid1 counter1 _ == RemoteFut' creatorPid2 counter2 _ = creatorPid1 == creatorPid2 && counter1 == counter2
  x == y = y == x -- swap args

instance Serializable a => Binary (Fut a) where
  put (LocalFut' creatorPid counter mvar) = unsafePerformIO $ modifyMVar futureStore' $ \ m -> 
      case M.lookup (creatorPid,counter) m of
        Nothing -> do
          forwarderPid <- forkProcess undefined $ do
                            res <- liftIO $ readMVar mvar
                            forever $ do
                              pid <- expect
                              send pid res
          return (m, put creatorPid *> put counter *> put forwarderPid)
        Just forwarderPid -> return (m, put creatorPid *> put counter *> put forwarderPid)
  put (RemoteFut' creatorPid counter forwarderPid) = put creatorPid *> put counter *> put forwarderPid
  get = RemoteFut' <$> get <*> get <*> get

  -- put o@(Obj' _ (Cog' _ _ pid _) counter) = do
  --   when (processNodeId pid == selfNode) $ 
  --     (unsafePerformIO (modifyMVar_ objectStore' (pure . M.insert (pid,counter) (AnyObj' o)))) `seq` pure ()
  --   put pid *> put counter
  -- get = do
  --     pid <- get
  --     counter <- get
  --     if processNodeId pid == selfNode
  --       then let m = M.lookup (pid,counter) $ unsafePerformIO (readMVar objectStore')
  --            in case m of
  --               -- or use the safer Data.Typeable.cast, or Data.Typeable.eqT
  --               Just (AnyObj' o) ->  -- unsafePerformIO (print "found foreign") `seq`
  --                                   pure (unsafeCoerce o :: Obj' a)
  --               _ -> fail "foreign table lookup fail"
  --       else pure $ Obj' undefined (Cog' undefined undefined pid undefined) counter



-- | an object reference is a pair of
-- 1) a reference to its cog
-- 2) its attributes placed in a mutable variable=IORef
-- NB: we deviate from ABS by not providing ordering of object-refs
data Obj' contents = Obj' (IORef contents) !Cog' Word


instance Binary (Obj' a) where
  put o@(Obj' _ (Cog' _ _ pid _) counter) = do
    when (processNodeId pid == selfNode) $ 
      (unsafePerformIO (modifyMVar_ objectStore' (pure . M.insert (pid,counter) (AnyObj' o)))) `seq` pure ()
    put pid *> put counter
  get = do
      pid <- get
      counter <- get
      if processNodeId pid == selfNode
        then let m = M.lookup (pid,counter) $ unsafePerformIO (readMVar objectStore')
             in case m of
                -- or use the safer Data.Typeable.cast, or Data.Typeable.eqT
                Just (AnyObj' o) ->  -- unsafePerformIO (print "found foreign") `seq`
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


{-# NOINLINE objectStore' #-}
objectStore' :: MVar (M.Map (ProcessId,Word) AnyObj')
objectStore' = unsafePerformIO $ newMVar M.empty
data AnyObj' = forall a. AnyObj' (Obj' a)

{-# NOINLINE futureStore' #-}
futureStore' :: MVar (M.Map (ProcessId,Word) ProcessId)
futureStore' = unsafePerformIO $ newMVar M.empty
