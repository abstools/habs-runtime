{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase #-}
module ABS.Runtime.Prim
  ( 
  -- * Creating new objects
  new, newlocal'
  -- * Synchronous method calling
  , sync', (<..>)
  -- * Asynchronous method calling
  , (<!>), (<!!>), awaitSugar', awaitSugar''
  -- * Cooperative yielding control
  , suspend
  -- * Awaiting on boolean conditions
  , awaitBool'
  -- * Awaiting on futures
  , awaitFuture', awaitFutures', awaitFutureField', ChangedFuture' (..)
  -- * Blocking on futures
  , get
  -- * Primitives for Timed-ABS extension
  -- | (using a soft-realtime interpretation)
  , currentms, now, duration, awaitDuration'
  -- * Lifting ABS pure code to ABS object layer

  -- | Haskell is pure by default. These are necessary functions for lifting pure ABS expressions (of the functional core) to the ABS' object layer (monadic statements).

  -- | Haskell's return is an expression taking a pure value and lifting it to the monadic world.
  , return
  -- * ABS strictness
  -- | Haskell is lazy be default. We use the HNF-strict version of 'fmap' ('<$>') to strictly pass the arguments to ABS methods and statements
  , (<$!>)
  -- * Other
  , skip, while, while', random, main_is', null, nullFuture',
  ) where

import ABS.Runtime.Base
import ABS.Runtime.CmdOpt
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)
import ABS.Runtime.Extension.Exception hiding (throw, catch)
import Control.Concurrent (ThreadId, myThreadId, newEmptyMVar, isEmptyMVar, putMVar, readMVar, forkIO, threadDelay, runInUnboundThread)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    

import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Monad.Trans.Class (lift)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (null)
import Control.Monad ((<$!>), when, unless, join)
import Control.Exception (Exception, throw, evaluate)
import Control.Monad.Catch (catch, catches, Handler (..))
import System.Clock (TimeSpec, getTime, Clock (Monotonic), toNanoSecs) -- for realtime
import qualified System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode (LineBuffering), hPutStrLn, hPrint, stderr, stdout)
import Data.Ratio (Ratio)
import Data.Typeable
import Data.List (delete)
import System.Random (randomRIO)
import Web.Scotty (ScottyM,scotty)

#ifdef WAIT_ALL_COGS
import Control.Exception (SomeException,try,mask)
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar')
import Foreign.StablePtr
{-# NOINLINE __tg #-}
-- | Not-exported
__tg :: TVar Int
__tg = unsafePerformIO $ newTVarIO 0

-- | Not-exported
forkIO__tg :: IO a -> IO ThreadId
forkIO__tg action = mask $ \restore -> do
    atomically $ modifyTVar' __tg (+ 1)
    forkIO $ 
      try (restore action) >>= \ (_ :: Either SomeException a) -> atomically (modifyTVar' __tg (subtract 1))
#endif

{-# NOINLINE __startClock #-}
-- | Not-exported
__startClock :: TimeSpec
__startClock = unsafePerformIO $ getTime Monotonic

-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)

-- | A static heap representation of the null object.
{-# NOINLINE null #-}
null :: Obj' Null'
null = Obj' (unsafePerformIO $ newIORef undefined) -- its object contents
            (Cog (throw NullException) (throw NullException)) -- its COG

-- | The uninitialized future.
--
-- Calling Fut<A> f; await f? will deadlock the current process.
-- Calling Fut<A> f; f.get; will deadlock the whole current-COG.
{-# NOINLINE nullFuture' #-}
nullFuture' :: Fut a
nullFuture' = unsafePerformIO $ newEmptyMVar

{-# INLINABLE suspend #-}
-- | Optimized suspend by avoiding capturing current-continuation if the method will be reactivated immediately:
-- implemented by inlining back' function and using a custom TQueue that exposes the otherwise abstract datatype TQueue.
suspend :: Obj' this -> ABS' ()
suspend (Obj' _ (Cog thisSleepTable thisMailBox@(TQueue tread twrite))) = do
  (mwoken, st') <- lift $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> do
      xs <- lift $ readTVarIO tread
      case xs of
        (k':ks') -> callCC (\ k -> do
                          lift $ do
                                 atomically $ writeTQueue thisMailBox (k ())
                                 atomically $ writeTVar tread ks'
                          k')
        [] -> do 
          ys <- lift $ readTVarIO twrite
          case ys of
            [] -> return ()       -- continue
            _  -> callCC (\ k -> join $
                            lift $ atomically $ do
                                 ws <- readTVar twrite
                                 case reverse (k():ws) of
                                   [] -> error "readTQueue"
                                   (z:zs) -> do writeTVar twrite []
                                                writeTVar tread zs
                                                return z
                         )
    Just woken -> callCC (\ k -> do
                           lift $ do
                             atomically $ writeTQueue thisMailBox (k ())                
                             writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                           woken)

{-# INLINE back' #-}
-- | Not-exported
back' :: Cog -> ABS' ()
back' (Cog thisSleepTable thisMailBox) = do
  (mwoken, st') <- lift $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> join $ lift $ atomically $ readTQueue thisMailBox
    Just woken -> do
                lift $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken
-- | Not-exported
handlers' :: [Handler ABS' a]
handlers' = [ Handler $ \ (ex :: AssertionFailed) -> lift $ hPrint stderr ex >> System.Exit.exitFailure
            , Handler $ \ (ex :: PatternMatchFail) -> do
                                          when (trace_exceptions cmdOpt) (lift $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
                                          return $ throw ex  -- rethrows it inside the future-"box"
            , Handler $ \ (ex :: RecSelError) -> do
                                          when (trace_exceptions cmdOpt) (lift $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
                                          return $ throw ex  -- rethrows it inside the future-"box"
            , Handler $ \ DivideByZero -> do
                                          when (trace_exceptions cmdOpt) (lift $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show DivideByZero)
                                          return $ throw DivideByZero  -- rethrows it inside the future-"box"
            , Handler $ \ (ex :: ABSException') -> do
                                          when (trace_exceptions cmdOpt) (lift $ hPutStrLn stderr $ "Process died upon Uncaught-Exception: " ++ show ex)
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
awaitFuture' (Obj' _ thisCog@(Cog _ thisMailBox)) fut = do
  empty <- lift $ isEmptyMVar fut -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  _ <- lift $ forkIO (do
                                    _ <- readMVar fut    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)

{-# INLINABLE awaitFutures' #-}
awaitFutures' :: Obj' this -> [IO Bool] -> IO a -> ABS' ()
awaitFutures' (Obj' _ thisCog@(Cog _ thisMailBox)) pollingTest blockingAction = do
  someEmpty <- lift $ orM pollingTest -- according to ABS' semantics it should continue right away, hence this test.
  when someEmpty $
    callCC (\ k -> do
                  _ <- lift $ forkIO (do
                                    _ <- blockingAction    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)
  where
    orM :: [IO Bool] -> IO Bool
    orM []          = pure False
    orM (p:ps)      = do
        q <- p
        if q
          then pure True
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
  empty <- lift $ isEmptyMVar mvar -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  lift $ forkIO (go mvar k) >>= \ tid -> modifyIORef' this' (setter (tid:))
                  back' thisCog)
  where
    go mvar' k = (do
                    _ <- readMVar mvar'    -- wait for future to be resolved
                    tid <- myThreadId
                    atomically $ writeTQueue thisMailBox (do
                        lift $ modifyIORef' this' (setter (delete tid))
                        k ())
                 ) `catch` (\ (ChangedFuture' mvar'') -> go mvar'' k)

data ChangedFuture' a = ChangedFuture' (Fut a)
instance Show (ChangedFuture' a) where show _ = "ChangedFuture' exception"
instance (Typeable a) => Exception (ChangedFuture' a)

{-# INLINABLE awaitBool' #-}
awaitBool' :: Obj' thisContents -> (thisContents -> Bool) -> ABS' ()
awaitBool' (Obj' thisContentsRef (Cog thisSleepTable thisMailBox)) testFun = do
  thisContents <- lift $ readIORef thisContentsRef
  unless (testFun thisContents) $
    callCC (\ k -> do
                       (mwoken, st') <- lift $ findWoken =<< readIORef thisSleepTable
                       lift $ writeIORef thisSleepTable $ 
                                  (testFun <$> readIORef thisContentsRef, k ()) : st' -- append failed await, maybe force like modifyIORef?
                       case mwoken of
                         Nothing -> join $ lift $ atomically $ readTQueue thisMailBox
                         Just woken -> woken
                       )

{-# INLINABLE awaitDuration' #-}
-- | in seconds, ignores second argument tmax
awaitDuration' :: Obj' this -> Ratio Int -> Ratio Int -> ABS' ()
awaitDuration' (Obj' _ thisCog@(Cog _ thisMailBox)) tmin _tmax = 
  callCC (\ k -> do
                  _ <- lift $ forkIO (do
                                    threadDelay $ truncate $ tmin * fromIntegral (fst (unit_time cmdOpt) *
                                      case snd $ unit_time cmdOpt of
                                        S -> 1000000
                                        Ms -> 1000
                                        Us -> 1)                        
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)


{-# INLINE sync' #-}
-- | sync
sync' :: Obj' this -> Obj' a -> (Obj' a -> ABS' r) -> ABS' r
sync' (Obj' _ (Cog _ thisMailBox)) callee@(Obj' _ (Cog _ otherMailBox)) methodPartiallyApplied = 
    if otherMailBox == thisMailBox
    then methodPartiallyApplied callee
    else lift $ get =<< (callee <!> methodPartiallyApplied) -- translated to an async+get, could be further optimized like awaitSugar' to keep 1 less thread)

{-# INLINE (<..>) #-}
-- | optimized sync, by not running same-cog-check. Used only by the generation when stumbled on "this.m()".
(<..>) :: Obj' a -> (Obj' a -> ABS' r) -> ABS' r
(<..>) obj methodPartiallyApplied = methodPartiallyApplied obj -- it is the reverse application

{-# INLINABLE (<!>) #-}
-- | async, unlifted
(<!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO (Fut b)
(<!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = do
  fut <- newEmptyMVar 
  atomically $ writeTQueue otherMailBox (do
                              res <- methodPartiallyApplied obj `catches` handlers'
                              lift $ putMVar fut res      -- method resolves future
                              back' otherCog)
  return fut                                                            


{-# INLINABLE (<!!>) #-}
-- | fire&forget async, unlifted
(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (do
               -- we throw away the result (if we had "destiny" primitive then this optimization could not be easily applied
               (() <$ methodPartiallyApplied obj) `catches` handlers'
               back' otherCog)

{-# INLINABLE awaitSugar' #-}
-- | Specialized (optimized support) for A res = await o!m().
--
-- The implementors could instead use a non-specialized version which desugars to
-- Fut<A> f_fresh = o!m(); await f_fresh; A res = f_fresh.get;
awaitSugar' :: Obj' this 
            -> (b -> IO ()) -- ^ LHS 
            -> Obj' a -- ^ callee
            -> (Obj' a -> ABS' b) -- ^ method 
            -> ABS' ()
awaitSugar' (Obj' _ thisCog@(Cog _ thisMailBox)) lhs obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  callCC (\ k -> do
    lift $ atomically $ writeTQueue otherMailBox (do
               res <- methodPartiallyApplied obj `catches` handlers'
               lift $ atomically $ writeTQueue thisMailBox (do
                                                              lift $ lhs $! res -- whnf to force the exception at the caller side TODO: to be tested , try also evaluate
                                                              k () )
               back' otherCog)
    back' thisCog)


{-# INLINABLE awaitSugar'' #-}
-- | Similar to 'awaitSugar'' but does not have a left-hand side, i.e. is of the form await o!m();. 
awaitSugar'' :: Obj' this 
            -> Obj' a -- ^ callee
            -> (Obj' a -> ABS' b) -- ^ method 
            -> ABS' ()
awaitSugar'' (Obj' _ thisCog@(Cog _ thisMailBox)) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  callCC (\ k -> do
    lift $ atomically $ writeTQueue otherMailBox (do
               _ <- methodPartiallyApplied obj `catches` handlers'
               lift $ atomically $ writeTQueue thisMailBox (k ())
               back' otherCog)
    back' thisCog)


{-# INLINABLE new #-}
-- | New object inside a newly-creted COG 
--
-- unlifted to ABS' monad
new :: (Obj' a -> IO ()) -> a -> IO (Obj' a)
new initFun objSmartCon = do
                -- create the cog
                newCogSleepTable <- newIORef []
                newCogMailBox <- newTQueueIO
                let newCog = Cog newCogSleepTable newCogMailBox

                -- create the object
                newObj'Contents <- newIORef objSmartCon
                let newObj' = Obj' newObj'Contents newCog

                -- create the init process on the new Cog
#ifdef WAIT_ALL_COGS
                _ <- forkIO__tg $ do
#else
                _ <- forkIO $ do
#endif
                            initFun newObj'
                            atomically (readTQueue newCogMailBox) >>= evalContT -- init method exits, does not have to findWoken because there can be no other processes yet
                return newObj'


{-# INLINABLE newlocal' #-}
-- | New object inside the currently-executing COG
--
-- unlifted to ABS' monad
newlocal' :: Obj' this -> (Obj' a -> IO ()) -> a -> IO (Obj' a)
newlocal' (Obj' _ thisCog) initFun objSmartCon = do
                -- create the object
                newObj'Contents <- newIORef objSmartCon
                let newObj' = Obj' newObj'Contents thisCog
                
                -- Safe optimization: we call init directly from here, safe because init cannot yield (has type IO)
                initFun newObj'

                return newObj'


{-# INLINE get #-}
-- | Block until the future has been resolved and try to extract the value.
-- May re-raise the exception of the original failed future.
-- unlifted to ABS' monad
get :: Fut b -> IO b
get fut = readMVar fut >>= evaluate -- forces to whnf, so as to for sure re-raise to the caller in case of exception-inside-future


-- |Not-exported
-- 
-- It has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (ABS' ()), SleepTable)
findWoken st = go st []
    where
      go [] rs = return (Nothing, rs)
      go (l@(t,k):ls) rs = t >>= \case
                                    True -> return (Just k, ls ++ rs)
                                    False -> go ls (l:rs)



{-# INLINE skip #-}
-- | Only as a showcase. The translator does not translate but __strips__ away skip statements.
skip :: ABS' ()
skip = return ()

-- | The ABS controlow flow. Takes a __mutable__ boolean condition that may contain fields (thus inside IO)
-- and an ABS block to execute (repeatedly).
while :: IO Bool -> ABS' () -> ABS' ()
while predAction loopAction = (`when` (loopAction >> while predAction loopAction)) =<< lift predAction -- else continue

-- | Specialized version of 'while' that is used only inside init blocks.
--
-- TODO: This version would be unnecessary if the 'while' was abstracted over any (MonadIO a)
while' :: IO Bool -> IO () -> IO ()
while' predAction loopAction = (`when` (loopAction >> while' predAction loopAction)) =<< predAction -- else continue


{-# INLINE currentms #-}
-- | Not much used. Baggage from the standard ABS library (abslang.abs)
currentms :: IO (Ratio Int)
currentms = (/ 1000000) . fromIntegral . toNanoSecs <$> getTime Monotonic

{-# INLINE now #-}
-- | Returns the current time of the real-time clock.
now :: IO TimeSpec
now = subtract __startClock <$> getTime Monotonic

{-# INLINE duration #-}
-- | in seconds, ignores second argument tmax
duration :: Ratio Int -> Ratio Int -> IO ()
duration tmin _tmax = threadDelay $ truncate $ tmin * fromIntegral (fst (unit_time cmdOpt) *
  case snd $ unit_time cmdOpt of
    S -> 1000000
    Ms -> 1000
    Us -> 1)


{-# INLINE random #-}
-- | Note: this is multicore-safe (thread-safe) but not multicore-scalable, because it uses underneath atomicModifyIORef
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
main_is' :: (Obj' contents -> ABS' ()) -> ScottyM () -> IO ()
main_is' mainABS' restAPI' = do
 _ <- evaluate __startClock -- force to compute the start clock of the program
 hSetBuffering stdout LineBuffering -- needed by EasyInterface's streaming. Default in linux-like is BlockBuffering
 hSetBuffering stderr LineBuffering -- needed by golden tests. Default in linux-like is NoBuffering
 case port cmdOpt of
    Just p -> forkIO (scotty p restAPI') >> pure ()
    _ -> pure ()
 runInUnboundThread $ do
#ifdef WAIT_ALL_COGS
  _ <- forkIO__tg $ do
#endif
      _ <- evaluate cmdOpt           -- force the cmdopt parsing, otherwise will not run even --help
      mb <- newTQueueIO
      st <- newIORef []
      evalContT $ do
        (mainABS' $ Obj' (error "runtime error: the main ABS' block tried to call 'this'") (Cog st mb)) `catches` handlers'
#ifdef WAIT_ALL_COGS
        back' (Cog st mb)
  t <- myThreadId
  s <- newStablePtr t
  atomically $ readTVar __tg >>= \n -> when (n >= 1) retry
  --freeStablePtr s
#endif