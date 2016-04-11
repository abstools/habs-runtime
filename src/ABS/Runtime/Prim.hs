module ABS.Runtime.Prim
    ( null
    , suspend, awaitFuture', awaitBool', get, emptyFuture
    , new, newlocal'
    , sync', (<..>), (<!>), (<!!>)
    , println, readln, skip, main_is'
    , while
    , assert
    ) where

import ABS.Runtime.Base
import ABS.Runtime.CmdOpt
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)

import Control.Concurrent (newEmptyMVar, isEmptyMVar, putMVar, readMVar, forkIO, runInUnboundThread)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    

import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (null)
import Control.Monad (liftM, when)
import Control.Exception (evaluate)
import qualified Control.Exception (assert)

-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)

{-# NOINLINE null #-}
null :: Obj' Null'
null = Obj' (unsafePerformIO $ newIORef undefined) -- its object contents
            (Cog (error "call to null object") (error "call to null object")) -- its COG

{-# INLINE suspend #-}
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
                                 atomically $ writeTQueue thisMailBox k
                                 atomically $ writeTVar tread ks'
                          k' ())
        [] -> do 
          ys <- liftIO $ readTVarIO twrite
          case ys of
            [] -> return ()       -- continue
            _  -> callCC (\ k -> do
                          k' <- liftIO $ atomically $ do
                                 ws <- readTVar twrite
                                 case reverse (k:ws) of
                                   [] -> error "readTQueue"
                                   (z:zs) -> do writeTVar twrite []
                                                writeTVar tread zs
                                                return z
                          k' ())
    Just woken -> callCC (\ k -> do
                           liftIO $ do
                             atomically $ writeTQueue thisMailBox k                
                             writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                           woken ())

{-# INLINE back' #-}
back' :: Cog -> ABS' ()
back' (Cog thisSleepTable thisMailBox) = do
  (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> do
                k <- liftIO $ atomically $ readTQueue thisMailBox
                k ()
    Just woken -> do
                liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken ()

-- Translation-time transformation
-- an await f? & x > 2 & g? & y < 4;
-- is broken to: (futures nub'ed)
-- awaitFuture f;
-- awaitFuture g;
-- 1 awaitBool (x > 2 && y < 4);

{-# INLINE awaitFuture' #-}
awaitFuture' :: Obj' this -> Fut a -> ABS' ()
awaitFuture' (Obj' _ thisCog@(Cog _ thisMailBox)) (Fut fut) = do
  empty <- liftIO $ isEmptyMVar fut -- according to ABS' semantics it should continue right away, hence this test.
  if empty
   then callCC (\ k -> do
                  _ <- liftIO $ forkIO (do
                                    _ <- readMVar fut    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox k)
                  back' thisCog)
   else return ()                   -- continue

{-# INLINE awaitBool' #-}
awaitBool' :: Obj' thisContents -> (thisContents -> Bool) -> ABS' ()
awaitBool' (Obj' thisContentsRef (Cog thisSleepTable thisMailBox)) testFun = do
  thisContents <- liftIO $ readIORef thisContentsRef
  if testFun thisContents
   then return () -- continue
   else callCC (\ k -> do
                       (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable
                       liftIO $ writeIORef thisSleepTable $ 
                                  (liftM testFun (readIORef thisContentsRef), k) : st' -- append failed await, maybe force like modifyIORef?
                       case mwoken of
                         Nothing -> do
                           k' <- liftIO $ atomically $ readTQueue thisMailBox
                           k' ()
                         Just woken -> woken ()
                       )

{-# INLINE emptyFuture #-}
-- | empty future unlifted
emptyFuture :: IO (Fut a)
emptyFuture = fmap Fut $ newEmptyMVar

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
  atomically $ writeTQueue otherMailBox (\ () -> do
                              res <- methodPartiallyApplied obj
                              liftIO $ putMVar fut res      -- method resolves future
                              back' otherCog)

  return (Fut fut)                                                            


{-# INLINE (<!!>) #-}
-- | fire&forget async, unlifted
(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (\ () -> do
               _ <- methodPartiallyApplied obj -- we throw away the result (if we had "destiny" primitive then we would need to create&resolve the future
               back' otherCog)



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
                            evalContT $ do
                               k <- liftIO $ atomically $ readTQueue newCogMailBox -- init method exits, does not have to findWoken because its sleeptable is empty
                               k ()

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
get (Fut fut) = readMVar fut


-- it has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (() -> ABS' ()), SleepTable)
findWoken st = findWoken' st Nothing []
    where
      findWoken' [] m st' = return (m, st')
      findWoken' (s@(t,k):st) Nothing st' = do
        b <- t 
        if b
          then findWoken' [] (Just k) (st ++ st')
          else findWoken' st Nothing (s:st')



{-# INLINE skip #-}
-- | Only only as a showcase. The translator does not translate but __strips__ away skip statements.
skip :: ABS' ()
skip = return ()


{-# INLINE println #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
println :: String -> ABS' ()
println = liftIO . putStrLn

{-# INLINE readln #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
readln :: ABS' String
readln = liftIO getLine

-- for init
{-# SPECIALIZE while :: IO Bool -> IO () -> IO () #-}
-- for rest
{-# SPECIALIZE while :: IO Bool -> ABS' () -> ABS' () #-}
while :: (MonadIO m) => IO Bool -> m () -> m ()
while predAction loopAction = do
  res <- liftIO predAction
  if res
   then loopAction >> while predAction loopAction
   else return ()                -- continue

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
assert :: Bool -> ABS' ()
assert b = Control.Exception.assert b (return ())
