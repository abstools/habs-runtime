{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module ABS.Runtime.Prim
    ( null, nullFuture'
    , suspend, awaitFuture', awaitBool', get
    , awaitFutures'
    , awaitFutureField', ChangedFuture' (..)
    , new, newlocal'
    , sync', (<..>), (<!>), (<!!>) --, awaitSugar'
    , skip
    , while, while'
    , (<$!>)
    -- * primitives for soft-realtime extension
    , now, duration, awaitDuration'
    , random
    , findWoken
    ) where

import ABS.Runtime.Base
import ABS.Runtime.CmdOpt
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)
import ABS.Runtime.Extension.Exception hiding (throw, catch)
import Control.Concurrent (ThreadId, myThreadId, newEmptyMVar, isEmptyMVar, putMVar, readMVar, forkIO, threadDelay, runInUnboundThread)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    

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
import qualified Data.IntMap as IM (empty, lookup, delete, insert, notMember)

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process (Process, spawnLocal, receiveWait, match, matchIf, matchSTM, send)
import Control.Distributed.Process.Node

-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)
  
{-# NOINLINE null #-}
null :: Obj' Null'
null = Obj' (unsafePerformIO $ newIORef undefined) -- its object contents
            (Cog (throw NullException) (throw NullException)) -- its COG

{-# NOINLINE nullFuture' #-}
nullFuture' :: Fut a
nullFuture' = unsafePerformIO $ newEmptyMVar

{-# INLINABLE suspend #-}
-- | Optimized suspend by avoiding capturing current-continuation if the method will be reactivated immediately:
-- implemented by inlining back' function and using a custom TQueue that exposes the otherwise abstract datatype TQueue.
suspend :: Obj' this -> ABS' ()
suspend (Obj' _ (Cog thisSleepTable thisMailBox@(TQueue tread twrite))) = do
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

--{-# INLINE back' #-}
--back' :: Cog -> ABS' ()
--back' (Cog thisSleepTable thisMailBox) = do
--  (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
--  case mwoken of
--    Nothing -> join $ liftIO $ atomically $ readTQueue thisMailBox
--    Just woken -> do
--                liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
--                woken

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
awaitFuture' (Obj' _ thisCog@(Cog _ thisMailBox)) mvar = do
  empty <- liftIO $ isEmptyMVar mvar -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    _ <- readMVar mvar    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)

{-# INLINABLE awaitFutures' #-}
awaitFutures' :: Obj' this -> [IO Bool] -> IO a -> ABS' ()
awaitFutures' (Obj' _ thisCog@(Cog _ thisMailBox)) pollingTest blockingAction = do
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
awaitFutureField' (Obj' this' thisCog@(Cog _ thisMailBox)) setter mvar = do
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
awaitBool' (Obj' thisContentsRef (Cog thisSleepTable thisMailBox)) testFun = do
  thisContents <- liftIO $ readIORef thisContentsRef
  unless (testFun thisContents) $
    callCC (\ k -> do
                       (mwoken, (bt',ft,ct)) <- liftIO $ findWoken =<< readIORef thisSleepTable
                       liftIO $ writeIORef thisSleepTable $ 
                                  ((testFun <$> readIORef thisContentsRef, k ()) : bt',ft,ct) -- append failed await, maybe force like modifyIORef?
                       case mwoken of
                         Nothing -> join $ liftIO $ atomically $ readTQueue thisMailBox
                         Just woken -> woken
                       )

{-# INLINABLE awaitDuration' #-}
-- | in seconds, ignores second argument tmax
awaitDuration' :: Obj' this -> Ratio Int -> Ratio Int -> ABS' ()
awaitDuration' (Obj' _ thisCog@(Cog _ thisMailBox)) tmin _tmax = 
  callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    threadDelay $ truncate $ tmin * 1000000 -- seconds to microseconds
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)


{-# INLINE sync' #-}
-- | sync
sync' :: Obj' this -> Obj' a -> (Obj' a -> ABS' r) -> ABS' r
sync' (Obj' _ (Cog _ thisCogToken)) callee@(Obj' _ (Cog _ calleeCogToken)) methodPartiallyApplied = 
    if calleeCogToken == thisCogToken
    then methodPartiallyApplied callee
    else throw SyncOnDifferentCOG

{-# INLINE (<..>) #-}
-- | optimized sync, by not running same-cog-check. Used only by the generation when stumbled on "this.m()".
(<..>) :: Obj' a -> (Obj' a -> ABS' r) -> ABS' r
(<..>) obj methodPartiallyApplied = methodPartiallyApplied obj -- it is the reverse application

{-# INLINABLE (<!>) #-}
-- | async, unliftIOed
(<!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO (Fut b)
(<!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = do
  mvar <- newEmptyMVar 
  atomically $ writeTQueue otherMailBox (do
                              res <- methodPartiallyApplied obj `catches` handlers'
                              liftIO $ putMVar mvar res      -- method resolves future
                              back' otherCog)
  return mvar                                                            


{-# INLINABLE (<!!>) #-}
-- | fire&forget async, unliftIOed
(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (do
               -- we throw away the result (if we had "destiny" primitive then this optimization could not be easily applied
               (() <$ methodPartiallyApplied obj) `catches` handlers'
               back' otherCog)

--{-# INLINABLE awaitSugar' #-}
---- | for await guard sugar
--awaitSugar' :: Obj' this 
--            -> (b -> IO ()) -- ^ LHS 
--            -> Obj' a -- ^ callee
--            -> (Obj' a -> ABS' b) -- ^ method 
--            -> ABS' ()
--awaitSugar' (Obj' _ thisCog@(Cog _ thisMailBox)) lhs obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
--  callCC (\ k -> do
--    liftIO $ atomically $ writeTQueue otherMailBox (do
--               res <- methodPartiallyApplied obj `catches` handlers'
--               liftIO $ atomically $ writeTQueue thisMailBox (do
--                                                              liftIO $ lhs $! res -- whnf to force the exception at the caller side TODO: to be tested , try also evaluate
--                                                              k () )
--               back' otherCog)
--    back' thisCog)



{-# INLINABLE new #-}
-- | new, unliftIOed
new :: (Obj' a -> IO ()) -> a -> Process (Obj' a)
new initFun objSmartCon = do
                -- create the cog
                newCogSleepTable <- liftIO $ newIORef ([], IM.empty, 0)
                newCogMailBox <- liftIO $ newTQueueIO
                let newCog = Cog newCogSleepTable newCogMailBox

                -- create the object
                newObj'Contents <- liftIO $ newIORef objSmartCon
                let newObj' = Obj' newObj'Contents newCog

                -- create the init process on the new Cog
                _ <- spawnLocal $ do
                            liftIO $ initFun newObj'
                            liftIO (atomically $ readTQueue newCogMailBox) >>= evalContT -- init method exits, does not have to findWoken because there can be no other processes yet
                return newObj'


{-# INLINABLE newlocal' #-}
-- | new local, unliftIOed
newlocal' :: Obj' this -> (Obj' a -> Process ()) -> a -> Process (Obj' a)
newlocal' (Obj' _ thisCog) initFun objSmartCon = do
                -- create the object
                newObj'Contents <- liftIO $ newIORef objSmartCon
                let newObj' = Obj' newObj'Contents thisCog
                
                -- Safe optimization: we call init directly from here, safe because init cannot yield (has type IO)
                initFun newObj'

                return newObj'


{-# INLINE get #-}
-- | get, unliftIOed
get :: Fut b -> IO b
get mvar = readMVar mvar >>= evaluate -- forces to whnf, so as to for sure re-raise to the caller in case of exception-inside-future

--get (Obj' _ (Cog thisSleepTable _)) (RemFut' i) = do
--  (bt,ft,ct) <- liftIO $ readIORef thisSleepTable
--  case IM.lookup i ft of -- TODO: can be optimized with IM.updateLookupWithKey
--    Just j -> do
--      liftIO $ writeIORef thisSleepTable (bt,IM.delete i ft,ct)
--      return j
--    Nothing -> receiveWait []
    
-- it has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (ABS' ()), SleepTable)
findWoken (bt,ft,ct) = go bt []
    where
      go [] rs = return (Nothing, (rs,ft,ct))
      go (l@(t,k):ls) rs = t >>= \case
                                    True -> return (Just k, (ls ++ rs,ft,ct))
                                    False -> go ls (l:rs)



{-# INLINE skip #-}
-- | Only only as a showcase. The translator does not translate but __strips__ away skip statements.
skip :: ABS' ()
skip = return ()


-- | for init
while' :: IO Bool -> Process () -> Process ()
while' predAction loopAction = (`when` (loopAction >> while' predAction loopAction)) =<< liftIO predAction -- else continue

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



get_i :: Obj' this -> RFut -> Process Int
get_i (Obj' _ (Cog thisSleepTable _)) rfut = do
  (bt,ft,ct) <- liftIO $ readIORef thisSleepTable
  case IM.lookup rfut ft of
    Just (res, _) -> do
      liftIO $ writeIORef thisSleepTable (bt, IM.delete rfut ft, ct)
      return res
    Nothing -> receiveWait [matchIf ((\case
                                        Right (_,rfut') -> rfut == rfut'
                                        _ -> False
                                    ) :: (Either (Int,Int) (Int,Int)) -> Bool) 
                                    (\ (Right (res,_)) -> 
                                      return res
                                    )
                           ]

request_i :: Obj' this -> Int -> RObj -> Process RFut
request_i (Obj' _ (Cog thisSleepTable _ )) param callee = do
  (bt, ft, ct) <- liftIO $ readIORef thisSleepTable
  liftIO $ writeIORef thisSleepTable (bt,ft,ct+1)
  callee `send` (Left (param, ct) :: Either (Int,Int) (Int,Int))
  return ct


await_i :: Obj' this -> RFut -> ABS' ()
await_i (Obj' _ thisCog@(Cog thisSleepTable _)) rfut = do
  (bt,ft,ct) <- liftIO $ readIORef thisSleepTable
  when (rfut `IM.notMember` ft) $ callCC (\ k -> do
    liftIO $ writeIORef thisSleepTable (bt, IM.insert rfut (undefined, k ()) ft, ct)
    back' thisCog
    )

back' :: Cog -> ABS' ()
back' thisCog@(Cog thisSleepTable thisMailBox) = do
  st@(bt,ft,ct) <- liftIO $ readIORef thisSleepTable
  (mwoken, st') <- liftIO $ findWoken st                                                  
  case mwoken of
    Nothing -> join $ lift $ receiveWait
      [
        match ((\case 
                -- request
                Left (param,rfut) -> undefined


                -- response
                Right (res,rfut) -> 
                  case IM.lookup rfut ft of
                    Just (_,k) -> do
                      liftIO $ writeIORef thisSleepTable (bt,IM.insert rfut (res,undefined) ft, ct)
                      return k
                    Nothing -> do
                      liftIO $ writeIORef thisSleepTable (bt,IM.insert rfut (res,undefined) ft, ct)
                      return (back' thisCog)
              ) :: Either (Int,Int) (Int,Int) -> Process (ABS' ()))
      , matchSTM (readTQueue thisMailBox) return
      ]
    Just woken -> do
                liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken

-- remotable
--new_i :: (Int,Int,ProcessId) -- worker class params + master 
--      -> Process ()
--new_i (workerId,size,master) = do
--  -- create the cog
--  newCogSleepTable <- liftIO $ newIORef ([], IM.empty, 0)
--  newCogMailBox <- liftIO $ newTQueueIO
--  let newCog = Cog newCogSleepTable newCogMailBox
--  -- create the object
--  newObj'Contents <- liftIO $ newIORef (smart_worker workerId size)
--  let newObj' = Obj' newObj'Contents newCog

--  -- the real init
--  liftIO $ init'Worker newObj'

--  -- the init_
--  (workers,rfut) <- expect :: Process ([RObj], RFut)
--  init_ workers newObj'
--  master `send` Right (-1,rfut)

--  -- the run_
--  rfut' <- expect :: Process RFut
--  run_ newObj'
--  master `send` Right (-2, rfut')
  
--  back' newCog 
