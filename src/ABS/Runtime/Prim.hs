module ABS.Runtime.Prim
    ( null
    , suspend, awaitFuture, awaitBool, get
    , new, newlocal
    , (<.>), (<!>), (<!!>)
    , println, readln, skip, main_is'
    ) where

import ABS.Runtime.Base

import Control.Concurrent (newMVar, newEmptyMVar, isEmptyMVar, putMVar, takeMVar, readMVar, forkIO, runInUnboundThread)
import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (null)
import Control.Monad (liftM)

-- this is fine but whenever it is used
-- we do (unsafeCoerce null :: MVar a) == d
-- this.a = unsafeCoerce (null)
-- (unsafeCoerce null ::MVar a) == (unsafeCoerce null :: MVar a)

{-# NOINLINE null #-}
null :: Obj a
null = Obj (error "call to null object") -- its COG
           (unsafePerformIO $ newIORef undefined) -- its object contents

{-# INLINE suspend #-}
suspend :: Obj this -> ABS ()
suspend (Obj (Cog thisToken thisSleepTable) _) = do
  (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
  case mwoken of
    Nothing -> liftIO $ do
      putMVar thisToken ()
      takeMVar thisToken
    Just woken -> callCC (\ k -> do
                           liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                           _ <- liftIO $ forkIO (evalContT $ do    -- delegate the current-continuation
                                                  liftIO $ takeMVar thisToken
                                                  k ())
                           woken ()) -- switch immediately to the woken process

-- Translation-time transformation
-- an await f? & x > 2 & g? & y < 4;
-- is broken to: (futures nub'ed)
-- awaitFuture f;
-- awaitFuture g;
-- 1 awaitBool (x > 2 && y < 4);

{-# INLINE awaitFuture #-}
awaitFuture :: Fut a -> Obj this -> ABS ()
awaitFuture (Fut fut) (Obj (Cog thisToken thisSleepTable) _) = do
  empty <- liftIO $ isEmptyMVar fut -- according to ABS semantics it should continue right away, hence this test.
  if empty
   then do
     (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable                                                 
     case mwoken of
       Nothing -> liftIO $ do
                putMVar thisToken () -- release cog (trying parked threads)
                _ <- readMVar fut    -- wait for future to be resolved
                takeMVar thisToken  -- acquire cog
       Just woken -> callCC (\ k -> do
                           liftIO $ writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                           _ <- liftIO $ forkIO (evalContT $ do    -- delegate the current-continuation
                                                  _ <- liftIO $ readMVar fut
                                                  liftIO $ takeMVar thisToken
                                                  k ())
                           woken ()) -- switch immediately to the woken process
    else return ()                   -- continue

{-# INLINE awaitBool #-}
awaitBool :: (thisContents -> Bool) -> Obj thisContents -> ABS ()
awaitBool testFun (Obj (Cog thisToken thisSleepTable) thisContentsRef) = do
  thisContents <- liftIO $ readIORef thisContentsRef
  if testFun thisContents
   then return () -- continue
   else callCC (\ k -> do
                       (mwoken, st') <- liftIO $ findWoken =<< readIORef thisSleepTable
                       liftIO $ writeIORef thisSleepTable $ 
                                  (liftM testFun (readIORef thisContentsRef), k) : st' -- append failed await, maybe force like modifyIORef?
                       case mwoken of
                         Nothing -> liftIO $ putMVar thisToken () -- release cog (trying parked threads)
                         Just woken -> woken ()
                       )

-- sync
{-# INLINE (<.>) #-}
(<.>) :: Obj a -> (Obj a -> ABS ()) -> Obj this -> ABS ()
(<.>) obj@(Obj (Cog objCogToken _) _) methodPartiallyApplied (Obj (Cog thisCogToken _) _) = 
    if objCogToken == thisCogToken
    then methodPartiallyApplied obj
    else error "SyncCalltoDifferentCOG"

{-# INLINE (<!>) #-}
-- async
(<!>) :: Obj a -> (Obj a -> ABS b) -> ABS (Fut b)
(<!>) obj@(Obj (Cog otherToken otherSleepTable) _) methodPartiallyApplied = liftIO $ do
  fut <- newEmptyMVar 
  _ <- forkIO $ do
        takeMVar otherToken
        evalContT $ do
                 res <- methodPartiallyApplied obj
                 liftIO $ putMVar fut res      -- method resolves future

                 (mwoken, st') <- liftIO $ findWoken =<< readIORef otherSleepTable                                                 
                 case mwoken of
                   Nothing -> liftIO $ putMVar otherToken () -- method exits
                   Just woken -> do
                           liftIO $ writeIORef otherSleepTable st' -- the sleep-table was modified, so write it back                               
                           woken ()

  return (Fut fut)                                                            


{-# INLINE (<!!>) #-}
-- fire&forget async
(<!!>) :: Obj a -> (Obj a -> ABS b) -> ABS ()
(<!!>) obj@(Obj (Cog otherToken otherSleepTable) _) methodPartiallyApplied = do
  _ <- liftIO $ forkIO $ do
        takeMVar otherToken
        evalContT $ do
                 _ <- methodPartiallyApplied obj -- we throw away the result (if we had "destiny" primitive then we would need to create&resolve the future

                 (mwoken, st') <- liftIO $ findWoken =<< readIORef otherSleepTable                                                 
                 case mwoken of
                   Nothing -> liftIO $ putMVar otherToken () -- method exits
                   Just woken -> do
                           liftIO $ writeIORef otherSleepTable st' -- the sleep-table was modified, so write it back                               
                           woken ()

  return ()


{-# INLINE new #-}
new :: a -> (Obj a -> ABS ()) -> ABS (Obj a)
new objSmartCon initFun = liftIO $ do
                -- create the cog
                newCogToken <- newMVar ()
                newCogSleepTable <- newIORef []
                let newCog = Cog newCogToken newCogSleepTable

                -- create the object
                newObjContents <- newIORef objSmartCon
                let newObj = Obj newCog newObjContents

                -- create the init process on the new Cog
                _ <- forkIO $ evalContT $ do
                                     initFun newObj
                                     liftIO $ putMVar newCogToken () -- init method exits, does not have to findWoken because its sleeptable is empty

                return newObj


{-# INLINE newlocal #-}
newlocal :: a -> (Obj a -> ABS ()) -> Obj this -> ABS (Obj a)
newlocal objSmartCon initFun (Obj thisCog _) = do
                -- create the object
                newObjContents <- liftIO $ newIORef objSmartCon
                let newObj = Obj thisCog newObjContents
                
                -- Safe optimization: we call init directly from here, safe because init is not allowed to yield
                initFun newObj

                return newObj


{-# INLINE get #-}
get :: Fut b -> ABS b
get (Fut fut) = liftIO $ takeMVar fut


-- it has to be in IO since it runs the read-obj-attr tests
findWoken :: SleepTable -> IO (Maybe (() -> ABS ()), SleepTable)
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
skip :: ABS ()
skip = return ()


{-# INLINE println #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
println :: String -> ABS ()
println = liftIO . putStrLn

{-# INLINE readln #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
readln :: ABS String
readln = liftIO readLn

-- {-# INLINE toString #-}
-- toString :: Show a => a -> String
-- toString = show

while' :: ABS Bool -> (ABS () -> ABS ()) -> ABS () -> ABS ()
while' predAction loopAction k = do
  res <- predAction
  if res
   then loopAction (while' predAction loopAction k)
   else k

-- for using inside ABS monad
{-# INLINE ifthenM' #-}
ifthenM' :: ABS Bool -> (ABS () -> ABS ()) -> ABS () -> ABS ()
ifthenM' texp stm_then k = texp >>= (\ e -> if e
                                           then stm_then k
                                           else k)

-- for using inside ABS monad
{-# INLINE ifthenelseM' #-}
ifthenelseM' :: ABS Bool -> (ABS () -> ABS ()) -> (ABS () -> ABS ()) -> ABS () -> ABS ()
ifthenelseM' texp stm_then stm_else k = texp >>= (\ e -> if e 
                                                       then stm_then k
                                                       else stm_else k)

-- for using inside OO code but in pure-exp, it is just lifted if-then-else
{-# INLINE ifthenelse' #-}
ifthenelse' :: ABS Bool -> ABS b -> ABS b -> ABS b
ifthenelse' p t e = do
  res <- p 
  if res
    then t
    else e

-- {-# INLINE main_is' #-}
-- main_is' :: (Obj Null -> (() -> ABS ()) -> ABS ()) -> IO () 
-- main_is' mainABS = runInUnboundThread $ do          -- wrapping in runInUnboundThread may increase the perf if main is not heavy
--   c <- newChan
--   pid <- myThreadId
--   let main_obj = ObjectRef undefined (COG (c, pid)) 0
--   -- send the Main Block as the 1st created process
--   writeChan c $ LocalJob $ mainABS main_obj (\ () -> lift (print "main finished" >> exitSuccess)) -- >> back__ main_obj) -- no explicit exit but expect to later (slower) make a check and auto-throw blockonmvarexception
--   S.evalStateT (back__ main_obj) (AState 1 M.empty M.empty)

main_is' :: (Obj contents -> ABS ()) -> IO ()
main_is' mainABS = runInUnboundThread $ evalContT $ do
  t <- liftIO $ newEmptyMVar
  st <- liftIO $ newIORef []
  mainABS (Obj (Cog t st) undefined)
  liftIO $ putMVar t ()                     
