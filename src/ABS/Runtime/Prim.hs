{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module ABS.Runtime.Prim
    ( null, nullFuture'
    , suspend, awaitFuture', awaitBool', get
    , awaitFutures'
    , awaitFutureField', ChangedFuture' (..)
    , new, newlocal'
    , sync', (<..>), (<!>), (<!!>), awaitSugar'
    , skip, main_is'
    , while, while'
    , (<$!>)
    -- * primitives for soft-realtime extension
    , now, duration, awaitDuration'
    , random
    ) where

import ABS.Runtime.Base
import ABS.Runtime.CmdOpt
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)
import ABS.Runtime.Extension.Exception hiding (throw, catch)
import Control.Concurrent (ThreadId, myThreadId, newEmptyMVar, isEmptyMVar, putMVar, readMVar, forkIO, threadDelay)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    
import Control.Distributed.Process (Process, NodeId(..), spawnLocal, receiveWait, unClosure, match, matchSTM, getSelfPid)
import Control.Distributed.Process.Node ( newLocalNode, initRemoteTable, runProcess)
import Control.Distributed.Process.Internal.Types (nullProcessId)
import Network.Transport.TCP (createTransport, defaultTCPParameters, encodeEndPointAddress)

import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (null)
import Control.Monad ((<$!>), when, unless, join)
import Control.Exception (Exception, SomeException, throw, evaluate)
import Control.Monad.Catch (catch, catches, Handler (..))
import Data.Time.Clock.POSIX (getPOSIXTime) -- for realtime
import qualified System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode (LineBuffering), hPutStrLn, hPrint, stderr)
import Data.Ratio (Ratio)
import Data.Typeable
import Data.List (delete)
import System.Random (randomRIO)


-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)
  
{-# NOINLINE null #-}
null :: Obj' Null'
null = Obj' (unsafePerformIO $ newIORef undefined) -- its object contents
            (Cog' (throw NullException) (throw NullException) (nullProcessId $ NodeId $ encodeEndPointAddress "0.0.0.0" "0" 0)) -- its COG

{-# NOINLINE nullFuture' #-}
nullFuture' :: Fut a
nullFuture' = unsafePerformIO $ newEmptyMVar

{-# INLINABLE suspend #-}
-- | Optimized suspend by avoiding capturing current-continuation if the method will be reactivated immediately:
-- implemented by inlining back' function and using a custom TQueue that exposes the otherwise abstract datatype TQueue.
suspend :: Obj' this -> ABS' ()
suspend (Obj' _ (Cog' thisSleepTable thisMailBox@(TQueue tread twrite) _)) = do
  (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> do
      xs <- liftIO $ readTVarIO tread
      case xs of
        (k':ks') -> callCC (\ k -> do
                          liftIO $ do
                                 atomically $ writeTQueue thisMailBox (k ())
                                 atomically $ writeTVar tread ks'
                          k')
        [] -> do 
          ys <- liftIO $ readTVarIO twrite
          case ys of
            [] -> return ()       -- continue
            _  -> callCC (\ k -> join $
                            liftIO $ atomically $ do
                                 ws <- readTVar twrite
                                 case reverse (k():ws) of
                                   [] -> error "readTQueue"
                                   (z:zs) -> do writeTVar twrite []
                                                writeTVar tread zs
                                                return z
                         )
    Just woken -> callCC (\ k -> do
                           liftIO $ do
                             atomically $ writeTQueue thisMailBox (k ())                
                             writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                           woken)

{-# INLINE back' #-}
back' :: Cog' -> ABS' ()
back' (Cog' thisSleepTable thisMailBox _) = do
  (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> join $ lift $ receiveWait
      [ match unClosure
      , matchSTM (readTQueue thisMailBox) pure
      ] 
    Just woken -> do
                liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken
handlers' :: [Handler ABS' a]
handlers' = [ Handler $ \ (ex :: AssertionFailed) -> liftIO $ hPrint stderr ex >> System.Exit.exitFailure
            , Handler $ \ (ex :: PatternMatchFail) -> do
                                          when (trace_exceptions cmdOpt) (liftIO $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
                                          return $ throw ex  -- rethrows it inside the future-"box"
            , Handler $ \ (ex :: RecSelError) -> do
                                          when (trace_exceptions cmdOpt) (liftIO $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
                                          return $ throw ex  -- rethrows it inside the future-"box"
            , Handler $ \ DivideByZero -> do
                                          when (trace_exceptions cmdOpt) (liftIO $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show DivideByZero)
                                          return $ throw DivideByZero  -- rethrows it inside the future-"box"
            , Handler $ \ (ex :: ABSException') -> do
                                          when (trace_exceptions cmdOpt) (liftIO $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
                                          return $ throw ex  -- rethrows it inside the future-"box"                                       
            ]

-- Translation-time transformation
-- an await f? & x > 2 & g? & y < 4;
-- is broken to: (futures nub'ed)
-- awaitFuture f;
-- awaitFuture g;
-- 1 awaitBool (x > 2 && y < 4);

{-# INLINABLE awaitFuture' #-}
awaitFuture' :: Obj' this -> Fut a -> ABS' ()
awaitFuture' (Obj' _ thisCog@(Cog' _ thisMailBox _)) fut = do
  empty <- liftIO $ isEmptyMVar fut -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    _ <- readMVar fut    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)

{-# INLINABLE awaitFutures' #-}
awaitFutures' :: Obj' this -> [IO Bool] -> IO a -> ABS' ()
awaitFutures' (Obj' _ thisCog@(Cog' _ thisMailBox _)) pollingTest blockingAction = do
  someEmpty <- liftIO $ orM pollingTest -- according to ABS' semantics it should continue right away, hence this test.
  when someEmpty $
    callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    _ <- blockingAction    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)

orM :: [IO Bool] -> IO Bool
orM []          = return False
orM (p:ps)      = do
        q <- p
        if q
          then return True
          else orM ps

---- taken from package monad-loops
--anyM :: (a -> IO Bool) -> [a] -> IO Bool
--anyM _ []       = return False
--anyM p (x:xs)   = do
--        q <- p x
--        if q
--          then return True
--          else anyM p xs

{-# INLINABLE awaitFutureField' #-}
awaitFutureField' :: Typeable a 
                  => Obj' this 
                  -> (([ThreadId] -> [ThreadId]) -> this -> this) 
                  -> Fut a 
                  -> ABS' ()
awaitFutureField' (Obj' this' thisCog@(Cog' _ thisMailBox _)) setter mvar = do
  empty <- liftIO $ isEmptyMVar mvar -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  liftIO $ forkIO (go mvar k) >>= \ tid -> modifyIORef' this' (setter (tid:))
                  back' thisCog)
  where
    go mvar' k = (do
                    _ <- readMVar mvar'    -- wait for future to be resolved
                    tid <- myThreadId
                    atomically $ writeTQueue thisMailBox (do
                        liftIO $ modifyIORef' this' (setter (delete tid))
                        k ())
                 ) `catch` (\ (ChangedFuture' mvar'') -> go mvar'' k)

data ChangedFuture' a = ChangedFuture' (Fut a)
instance Show (ChangedFuture' a) where show _ = "ChangedFuture' exception"
instance (Typeable a) => Exception (ChangedFuture' a)

{-# INLINABLE awaitBool' #-}
awaitBool' :: Obj' thisContents -> (thisContents -> Bool) -> ABS' ()
awaitBool' (Obj' thisContentsRef (Cog' thisSleepTable thisMailBox _)) testFun = do
  thisContents <- liftIO $ readIORef thisContentsRef
  unless (testFun thisContents) $
    callCC (\ k -> do
                       (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable
                       liftIO $ writeIORef thisSleepTable $ 
                                  (testFun <$> readIORef thisContentsRef, k ()) : st' -- append failed await, maybe force like modifyIORef?
                       case mwoken of
                         Nothing -> join $ liftIO $ atomically $ readTQueue thisMailBox
                         Just woken -> woken
                       )

{-# INLINABLE awaitDuration' #-}
-- | in seconds, ignores second argument tmax
awaitDuration' :: Obj' this -> Ratio Int -> Ratio Int -> ABS' ()
awaitDuration' (Obj' _ thisCog@(Cog' _ thisMailBox _)) tmin _tmax = 
  callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    threadDelay $ truncate $ tmin * 1000000 -- seconds to microseconds
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)


{-# INLINE sync' #-}
-- | sync
sync' :: Obj' this -> Obj' a -> (Obj' a -> ABS' r) -> ABS' r
sync' (Obj' _ (Cog' _ thisCogToken _)) callee@(Obj' _ (Cog' _ calleeCogToken _)) methodPartiallyApplied = 
    if calleeCogToken == thisCogToken
    then methodPartiallyApplied callee
    else throw SyncOnDifferentCOG

{-# INLINE (<..>) #-}
-- | optimized sync, by not running same-cog-check. Used only by the generation when stumbled on "this.m()".
(<..>) :: Obj' a -> (Obj' a -> ABS' r) -> ABS' r
(<..>) obj methodPartiallyApplied = methodPartiallyApplied obj -- it is the reverse application

{-# INLINABLE (<!>) #-}
-- | async, unlifted
(<!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO (Fut b)
(<!>) obj@(Obj' _ otherCog@(Cog' _ otherMailBox _)) methodPartiallyApplied = do
  fut <- newEmptyMVar 
  atomically $ writeTQueue otherMailBox (do
                              res <- methodPartiallyApplied obj `catches` handlers'
                              liftIO $ putMVar fut res      -- method resolves future
                              back' otherCog)
  return fut                                                            


{-# INLINABLE (<!!>) #-}
-- | fire&forget async, unlifted
(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog' _ otherMailBox _)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (do
               -- we throw away the result (if we had "destiny" primitive then this optimization could not be easily applied
               (() <$ methodPartiallyApplied obj) `catches` handlers'
               back' otherCog)

{-# INLINABLE awaitSugar' #-}
-- | for await guard sugar
awaitSugar' :: Obj' this 
            -> (b -> IO ()) -- ^ LHS 
            -> Obj' a -- ^ callee
            -> (Obj' a -> ABS' b) -- ^ method 
            -> ABS' ()
awaitSugar' (Obj' _ thisCog@(Cog' _ thisMailBox _)) lhs obj@(Obj' _ otherCog@(Cog' _ otherMailBox _)) methodPartiallyApplied = 
  callCC (\ k -> do
    liftIO $ atomically $ writeTQueue otherMailBox (do
               res <- methodPartiallyApplied obj `catches` handlers'
               liftIO $ atomically $ writeTQueue thisMailBox (do
                                                              liftIO $ lhs $! res -- whnf to force the exception at the caller side TODO: to be tested , try also evaluate
                                                              k () )
               back' otherCog)
    back' thisCog)




{-# INLINABLE new #-}
-- | new, unlifted
new :: (Obj' a -> Process ()) -> a -> Process (Obj' a)
new initFun objSmartCon = do
                -- create the cog
                newCogSleepTable <- liftIO $ newIORef []
                newCogMailBox <- liftIO $ newTQueueIO

                -- create the object
                newObj'Contents <- liftIO $ newIORef objSmartCon

                -- create the init process on the new Cog
                newPid <- spawnLocal $ do
                            self <- getSelfPid
                            initFun (Obj' newObj'Contents (Cog' newCogSleepTable newCogMailBox self))
                            evalContT =<< receiveWait
                                            [ match unClosure
                                            , matchSTM (readTQueue newCogMailBox) pure
                                            ]
                            -- init method exits, does not have to findWoken because there can be no other processes yet
                return (Obj' newObj'Contents (Cog' newCogSleepTable newCogMailBox newPid))


{-# INLINABLE newlocal' #-}
-- | new local, unlifted
newlocal' :: Obj' this -> (Obj' a -> Process ()) -> a -> Process (Obj' a)
newlocal' (Obj' _ thisCog) initFun objSmartCon = do
                -- create the object
                newObj'Contents <- liftIO $ newIORef objSmartCon
                let newObj' = Obj' newObj'Contents thisCog
                
                -- Safe optimization: we call init directly from here, safe because init cannot yield (has type IO)
                initFun newObj'

                return newObj'


{-# INLINE get #-}
-- | get, unlifted
get :: Fut b -> IO b
get fut = readMVar fut >>= evaluate -- forces to whnf, so as to for sure re-raise to the caller in case of exception-inside-future


-- it has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (ABS' ()), SleepTable)
findWoken st = go st []
    where
      go [] rs = return (Nothing, rs)
      go (l@(t,k):ls) rs = t >>= \case
                                    True -> return (Just k, ls ++ rs)
                                    False -> go ls (l:rs)



{-# INLINE skip #-}
-- | Only only as a showcase. The translator does not translate but __strips__ away skip statements.
skip :: ABS' ()
skip = return ()


-- | for init
while' :: IO Bool -> IO () -> IO ()
while' predAction loopAction = (`when` (loopAction >> while' predAction loopAction)) =<< predAction -- else continue

while :: IO Bool -> ABS' () -> ABS' ()
while predAction loopAction = (`when` (loopAction >> while predAction loopAction)) =<< liftIO predAction -- else continue
 

{-# INLINE now #-}
now :: IO Time
now = getPOSIXTime

{-# INLINE duration #-}
-- | in seconds, ignores second argument tmax
duration :: Ratio Int -> Ratio Int -> IO ()
duration tmin _tmax = threadDelay $ truncate $ tmin * 1000000


{-# INLINE random #-}
-- | Note: this is multicore-safe but not multicore-scalable, because it uses underneath atomicModifyIORef
random :: Int -> IO Int
random i = randomRIO (0, case compare i 0 of
                            GT -> i-1
                            EQ -> 0
                            LT -> i+1)

{-# INLINE main_is' #-}
-- | This function takes an ABS'' main function in the module and executes the ABS' program.
--
-- Note the mainABS' function expects a this object as input. This is only for unifying the method-block generation;
-- the code-generator will safely catch if a main contains calls to this. This runtime, however, does not do such checks;
-- if the user passes a main that uses this, the program will err.
main_is' :: (Obj' contents -> ABS' ()) -> IO ()
main_is' mainABS' = do
 hSetBuffering stderr LineBuffering
 Right t <- createTransport (ip cmdOpt) (port cmdOpt) defaultTCPParameters
 node <- newLocalNode t initRemoteTable
 mb <- newTQueueIO
 st <- newIORef []
 runProcess node $ 
  case creator cmdOpt of
    [] -> do
      self <- getSelfPid
      evalContT $ (mainABS' $ Obj' (error "runtime error: the main ABS' block tried to call 'this'") (Cog' st mb self)) `catches` handlers'
    _ -> undefined