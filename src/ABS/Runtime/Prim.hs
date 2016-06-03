module ABS.Runtime.Prim
    ( null, nullFuture'
    , suspend, awaitFuture', awaitBool', get
    , new, newlocal'
    , sync', (<..>), (<!>), (<!!>), awaitSugar'
    , println, readln, skip, main_is'
    , while, while'
    , assert
    , (<$!>)
    -- * primitives for soft-realtime extension
    , now, duration, awaitDuration'
    ) where

import ABS.Runtime.Base
import ABS.Runtime.CmdOpt
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)
import ABS.Runtime.Extension.Exception hiding (throw) -- we use here the evaluation throw, not the ordering throwIO (throwM)

import Control.Concurrent (newEmptyMVar, isEmptyMVar, putMVar, readMVar, forkIO, threadDelay, runInUnboundThread)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    

import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Monad.Trans.Class (lift)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (null)
import Control.Monad ((<$!>), when, unless, join)
import Control.Exception (throw, evaluate)
import qualified Control.Exception (assert)
import Control.Monad.Catch (catchAll)
import Data.Time.Clock.POSIX (getPOSIXTime) -- for realtime

-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)
  
{-# NOINLINE null #-}
null :: Obj' Null'
null = Obj' (unsafePerformIO $ newIORef undefined) -- its object contents
            (Cog (error "call to null object") (error "call to null object")) -- its COG

{-# NOINLINE nullFuture' #-}
nullFuture' :: Fut a
nullFuture' = Fut (unsafePerformIO $ newEmptyMVar)

{-# INLINE suspend #-}
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
back' :: Cog -> ABS' ()
back' (Cog thisSleepTable thisMailBox) = do
  (mwoken, st') <- lift $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> join $ lift $ atomically $ readTQueue thisMailBox
    Just woken -> do
                lift $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken

-- Translation-time transformation
-- an await f? & x > 2 & g? & y < 4;
-- is broken to: (futures nub'ed)
-- awaitFuture f;
-- awaitFuture g;
-- 1 awaitBool (x > 2 && y < 4);

{-# INLINE awaitFuture' #-}
awaitFuture' :: Obj' this -> Fut a -> ABS' ()
awaitFuture' (Obj' _ thisCog@(Cog _ thisMailBox)) (Fut fut) = do
  empty <- lift $ isEmptyMVar fut -- according to ABS' semantics it should continue right away, hence this test.
  when empty $
    callCC (\ k -> do
                  _ <- lift $ forkIO (do
                                    _ <- readMVar fut    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)

{-# INLINE awaitBool' #-}
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

{-# INLINE awaitDuration' #-}
-- | in seconds, ignores second argument tmax
awaitDuration' :: Obj' this -> Rational -> Rational -> ABS' ()
awaitDuration' (Obj' _ thisCog@(Cog _ thisMailBox)) tmin _tmax = 
  callCC (\ k -> do
                  _ <- lift $ forkIO (do
                                    threadDelay $ truncate $ tmin * 1000000 -- seconds to microseconds
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' thisCog)


{-# INLINE sync' #-}
-- | sync
sync' :: Obj' this -> Obj' a -> (Obj' a -> ABS' r) -> ABS' r
sync' (Obj' _ (Cog _ thisCogToken)) callee@(Obj' _ (Cog _ calleeCogToken)) methodPartiallyApplied = 
    if calleeCogToken == thisCogToken
    then methodPartiallyApplied callee
    else error "SyncCalltoDifferentCOG"

{-# INLINE (<..>) #-}
-- | optimized sync, by not running same-cog-check. Used only by the generation when stumbled on "this.m()".
(<..>) :: Obj' a -> (Obj' a -> ABS' r) -> ABS' r
(<..>) obj methodPartiallyApplied = methodPartiallyApplied obj -- it is the reverse application

{-# INLINE (<!>) #-}
-- | async, unlifted
(<!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO (Fut b)
(<!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = do
  fut <- newEmptyMVar 
  atomically $ writeTQueue otherMailBox (do
                              res <- methodPartiallyApplied obj `catchAll` (\ someEx -> do
                                          when (trace_exceptions cmdOpt) (lift $ putStrLn $ "Process died upon Uncaught-Exception: " ++ show someEx)
                                          return $ throw someEx)  -- rethrows it inside the future-"box"                                       
                            
                              lift $ putMVar fut res      -- method resolves future
                              back' otherCog)

  return (Fut fut)                                                            


{-# INLINE (<!!>) #-}
-- | fire&forget async, unlifted
(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (do
               -- we throw away the result (if we had "destiny" primitive then this optimization could not be easily applied
               (() <$ methodPartiallyApplied obj) `catchAll` (\ someEx ->
                                when (trace_exceptions cmdOpt) (lift $ putStrLn $ "Process died upon Uncaught-Exception: " ++ show someEx))
               back' otherCog)

{-# INLINE awaitSugar' #-}
-- | for await guard sugar
awaitSugar' :: Obj' this 
            -> (b -> IO ()) -- ^ LHS 
            -> Obj' a -- ^ callee
            -> (Obj' a -> ABS' b) -- ^ method 
            -> ABS' ()
awaitSugar' (Obj' _ thisCog@(Cog _ thisMailBox)) lhs obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  callCC (\ k -> do
    lift $ atomically $ writeTQueue otherMailBox (do
               res <- methodPartiallyApplied obj `catchAll` (\ someEx -> do
                                          when (trace_exceptions cmdOpt) (lift $ putStrLn $ "Process died upon Uncaught-Exception: " ++ show someEx)
                                          return $ throw someEx)  -- rethrows it inside the future-"box"
               lift $ atomically $ writeTQueue thisMailBox (do
                                                              lift $ lhs $! res -- whnf to force the exception at the caller side TODO: to be tested , try also evaluate
                                                              k () )
               back' otherCog)
    back' thisCog)


--{-# INLINE awaitSugar #-}
---- | for await guard sugar
--awaitSugar :: Obj' this -> Obj' a -> (Obj' a -> ABS' b) -> ABS' b
--awaitSugar (Obj' _ thisCog@(Cog _ thisMailBox)) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
--  callCC (\ k -> do
--    lift $ atomically $ writeTQueue otherMailBox (do
--               res <- methodPartiallyApplied obj
--               lift $ atomically $ writeTQueue thisMailBox (k res)
--               back' otherCog)
--    back' thisCog
--  )



{-# INLINE new #-}
-- | new, unlifted
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
                _ <- forkIO $ do
                            initFun newObj'
                            atomically (readTQueue newCogMailBox) >>= evalContT -- init method exits, does not have to findWoken because there can be no other processes yet
                return newObj'


{-# INLINE newlocal' #-}
-- | new local, unlifted
newlocal' :: Obj' this -> (Obj' a -> IO ()) -> a -> IO (Obj' a)
newlocal' (Obj' _ thisCog) initFun objSmartCon = do
                -- create the object
                newObj'Contents <- newIORef objSmartCon
                let newObj' = Obj' newObj'Contents thisCog
                
                -- Safe optimization: we call init directly from here, safe because init cannot yield (has type IO)
                initFun newObj'

                return newObj'


{-# INLINE get #-}
-- | get, unlifted
get :: Fut b -> IO b
get (Fut fut) = readMVar fut >>= evaluate -- forces to whnf, so as to for sure re-raise to the caller in case of exception-inside-future


-- it has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (ABS' ()), SleepTable)
findWoken st = findWoken' st Nothing []
    where
      findWoken' [] m st' = return (m, st')
      findWoken' (s@(t,k):st) Nothing st' = do
        b <- t 
        if b
          then return (Just k, st ++ st')
          else findWoken' st Nothing (s:st')



{-# INLINE skip #-}
-- | Only only as a showcase. The translator does not translate but __strips__ away skip statements.
skip :: ABS' ()
skip = return ()


{-# INLINE println #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
println :: String -> ABS' ()
println = lift . putStrLn

{-# INLINE readln #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
readln :: ABS' String
readln = lift getLine

-- | for init
while' :: IO Bool -> IO () -> IO ()
while' predAction loopAction = (`when` (loopAction >> while' predAction loopAction)) =<< predAction -- else continue

while :: IO Bool -> ABS' () -> ABS' ()
while predAction loopAction = (`when` (loopAction >> while predAction loopAction)) =<< lift predAction -- else continue
 

{-# INLINE now #-}
now :: IO Time
now = getPOSIXTime

{-# INLINE duration #-}
-- | in seconds, ignores second argument tmax
duration :: Rational -> Rational -> IO ()
duration tmin _tmax = threadDelay $ truncate $ tmin * 1000000


{-# INLINE main_is' #-}
-- | This function takes an ABS'' main function in the module and executes the ABS' program.
--
-- Note the mainABS' function expects a this object as input. This is only for unifying the method-block generation;
-- the code-generator will safely catch if a main contains calls to this. This runtime, however, does not do such checks;
-- if the user passes a main that uses this, the program will err.
main_is' :: (Obj' contents -> ABS' ()) -> IO ()
main_is' mainABS' = runInUnboundThread $ do
  _ <- evaluate cmdOpt           -- force the cmdopt parsing, otherwise will not run even --help
  mb <- newTQueueIO
  st <- newIORef []
  evalContT $ do
    mainABS' $ Obj' (error "runtime error: the main ABS' block tried to call 'this'") (Cog st mb) 
    when (keep_alive cmdOpt) $ back' (Cog st mb) -- if we want the main not to exit too early, we pass keep-alive that keeps the ABS' program alive forever

{-# INLINE assert #-}
assert :: Bool -> IO ()
assert b = Control.Exception.assert b (return ())


