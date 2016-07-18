{-# LINE 1 "..\pa\src\DisPA.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module DisPA (main) where

import ABS.Runtime.Base
import ABS.Runtime.Prim (findWoken, while, get, (<$!>), null, nullFuture',awaitBool',(<..>))
import ABS.Runtime.Extension.Promise
import ABS.Runtime.Extension.IO
import ABS.Runtime.CmdOpt

import ABS.StdLib
import Data.Function ((.))
import Control.Applicative ((<*>), (*>))
import Control.Monad ((=<<))
import qualified Control.Applicative as I' (pure)
import qualified Data.IORef as I' (newIORef, readIORef, writeIORef)
import qualified Control.Monad.IO.Class as I' (liftIO)
import qualified Control.Monad.Trans.Class as I' (lift)
import qualified Control.Monad as I' (when, sequence, join)
import qualified Prelude as I'
       (IO, Eq, Ord(..), Show(..), undefined, error, negate, fromIntegral,
        mapM_)
import qualified Unsafe.Coerce as I' (unsafeCoerce)
import qualified Control.Concurrent as I' (ThreadId)
import qualified Control.Concurrent.MVar as I'
       (isEmptyMVar, readMVar,newEmptyMVar,putMVar)
import Control.Exception (assert)
import qualified Control.Exception as I'
       (Exception(..), SomeException, throwTo, throw)
{-# LINE 4 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (IOVector(..))
{-# LINE 5 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (unsafeNew)
{-# LINE 6 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (read)
{-# LINE 7 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (write)
{-# LINE 10 "..\pa\src\DisPA.abs" #-}
import Data.Vector (Vector(..))
{-# LINE 11 "..\pa\src\DisPA.abs" #-}
import Data.Vector (fromList)
{-# LINE 12 "..\pa\src\DisPA.abs" #-}
import Data.Vector (indexM)
{-# LINE 15 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (GenIO(..))
{-# LINE 16 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (createSystemRandom)
{-# LINE 17 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (uniformR)
{-# LINE 20 "..\pa\src\DisPA.abs" #-}
import System.Clock (getTime)
{-# LINE 21 "..\pa\src\DisPA.abs" #-}
import System.Clock (diffTimeSpec)
{-# LINE 22 "..\pa\src\DisPA.abs" #-}
import System.Clock (Clock(..))
{-# LINE 23 "..\pa\src\DisPA.abs" #-}
import System.Clock (TimeSpec(..))
{-# LINE 26 "..\pa\src\DisPA.abs" #-}
import GHC.Conc (numCapabilities)
{-# LINE 29 "..\pa\src\DisPA.abs" #-}
import System.IO.Unsafe (unsafePerformIO)
{-# LINE 32 "..\pa\src\DisPA.abs" #-}
import Prelude (quot)
{-# LINE 33 "..\pa\src\DisPA.abs" #-}
import Prelude (rem, ($))
import Data.List ((++))

import Control.Monad.Trans.Cont (evalContT, callCC)
import qualified Data.IntMap as IM (empty, lookup, delete, insert, notMember)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process (Process, spawnLocal, receiveWait, match, matchIf, matchSTM, send, expect, ProcessId,getSelfPid)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import ABS.Runtime.TQueue (TQueue (..), newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)    
import Control.Applicative ((<$))
import Control.Concurrent (forkIO)

default (Int, Rat)

{-# INLINABLE (<!>) #-}
-- | async, unliftIOed
--(<!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO (Fut b)
(<!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = do
  mvar <- I'.newEmptyMVar 
  atomically $ writeTQueue otherMailBox (do
                              res <- methodPartiallyApplied obj
                              I'.liftIO $ I'.putMVar mvar res      -- method resolves future
                              back' obj)
  return mvar                                                            


{-# INLINABLE (<!!>) #-}
-- | fire&forget async, unliftIOed
--(<!!>) :: Obj' a -> (Obj' a -> ABS' b) -> IO ()
(<!!>) obj@(Obj' _ otherCog@(Cog _ otherMailBox)) methodPartiallyApplied = 
  atomically $ writeTQueue otherMailBox (do
               -- we throw away the result (if we had "destiny" primitive then this optimization could not be easily applied
               (() <$ methodPartiallyApplied obj)
               back' obj)


{-# INLINABLE awaitFuture' #-}
--awaitFuture' :: Obj' this -> Fut a -> ABS' ()
awaitFuture' obj@(Obj' _ thisCog@(Cog _ thisMailBox)) mvar = do
  empty <- I'.liftIO $ I'.isEmptyMVar mvar -- according to ABS' semantics it should continue right away, hence this test.
  I'.when empty $
    callCC (\ k -> do
                  _ <- I'.liftIO $ forkIO (do
                                    _ <- I'.readMVar mvar    -- wait for future to be resolved
                                    atomically $ writeTQueue thisMailBox (k ()))
                  back' obj)


{-# INLINE main_is' #-}
-- | This function takes an ABS'' main function in the module and executes the ABS' program.
--
-- Note the mainABS' function expects a this object as input. This is only for unifying the method-block generation;
-- the code-generator will safely catch if a main contains calls to this. This runtime, however, does not do such checks;
-- if the user passes a main that uses this, the program will err.
--main_is' :: (Obj' contents -> ABS' ()) -> IO ()
main_is' mainABS' = do
 backend <- initializeBackend (ip cmdOpt) (port cmdOpt) initRemoteTable
 if master cmdOpt
  then do
    mb <- newTQueueIO
    st <- I'.newIORef ([],IM.empty,0)     
    startMaster backend (\ peers -> do
      I'.liftIO (print $ "Slaves:" ++ I'.show peers)
      evalContT $ (mainABS' $ Obj' (I'.error "runtime error: the main ABS' block tried to call 'this'") (Cog st mb)))
  else startSlave backend

 --Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
 --node <- newLocalNode t initRemoteTable
 




get_i :: Obj' this -> RFut -> Process Int
get_i (Obj' _ (Cog thisSleepTable _)) rfut = do
  (bt,ft,ct) <- I'.liftIO $ I'.readIORef thisSleepTable
  case IM.lookup rfut ft of
    Just (res, _) -> do
      I'.liftIO $ I'.writeIORef thisSleepTable (bt, IM.delete rfut ft, ct)
      return res
    Nothing -> receiveWait [matchIf ((\ (_,rfut') -> rfut == rfut') :: (Int,Int) -> Bool) 
                                    (\ (res,_) -> return res)
                           ]

request_i :: Obj' this -> Int -> RObj -> Process RFut
request_i (Obj' _ (Cog thisSleepTable _ )) param callee = do
  (bt, ft, ct) <- I'.liftIO $ I'.readIORef thisSleepTable
  I'.liftIO $ I'.writeIORef thisSleepTable (bt,ft,ct+1)
  self <- getSelfPid
  callee `send` (self, param, ct)
  return ct


await_i :: Obj' this -> RFut -> ABS' ()
await_i obj@(Obj' _ thisCog@(Cog thisSleepTable _)) rfut = do
  (bt,ft,ct) <- I'.liftIO $ I'.readIORef thisSleepTable
  I'.when (rfut `IM.notMember` ft) $ callCC (\ k -> do
    I'.liftIO $ I'.writeIORef thisSleepTable (bt, IM.insert rfut (I'.undefined, k ()) ft, ct)
    back' obj
    )

back' :: Obj' this -> ABS' ()
back' obj@(Obj' _ thisCog@(Cog thisSleepTable thisMailBox)) = do
  st@(bt,ft,ct) <- I'.liftIO $ I'.readIORef thisSleepTable
  (mwoken, st') <- I'.liftIO $ findWoken st                                                  
  case mwoken of
    Nothing -> I'.join $ I'.lift $ receiveWait
      [
        match ((\ 
                -- request
                (caller,param,rfut) -> do
                  res <- request param obj 
                  caller `send` (res,rfut)
                ))
      , match ((\ 
                -- response
                (res,rfut) -> 
                  case IM.lookup rfut ft of
                    Just (_,k) -> do
                      I'.liftIO $ I'.writeIORef thisSleepTable (bt,IM.insert rfut (res,I'.undefined) ft, ct)
                      return k
                    Nothing -> do
                      I'.liftIO $ I'.writeIORef thisSleepTable (bt,IM.insert rfut (res,I'.undefined) ft, ct)
                      return (back' obj)
              )) -- :: Either (Int,Int) (Int,Int) -> Process (ABS' ()))
      , matchSTM (readTQueue thisMailBox) return
      ]
    Just woken -> do
                I'.liftIO $ I'.writeIORef thisSleepTable st' -- the sleep-table was modified, so write it back
                woken

-- remotable
new_i :: (Int,Int,ProcessId) -- worker class params + master 
      -> Process ()
new_i (workerId,size,master) = do
  -- create the cog
  newCogSleepTable <- I'.liftIO $ I'.newIORef ([], IM.empty, 0)
  newCogMailBox <- I'.liftIO $ newTQueueIO
  let newCog = Cog newCogSleepTable newCogMailBox
  -- create the object
  newObj'Contents <- I'.liftIO $ I'.newIORef (smart'Worker workerId size)
  let newObj' = Obj' newObj'Contents newCog

  -- the real init
  I'.liftIO $ init'Worker newObj'

  -- the init_
  (workers,rfut) <- expect :: Process ([RObj], RFut)
  --init_ workers newObj'
  master `send` (-1,rfut)

  -- the run_
  rfut' <- expect :: Process RFut
  --run_ newObj'
  master `send` (-2, rfut')
  
  back' newObj' 


div :: _ => Int -> Int -> Int
{-# LINE 36 "..\pa\src\DisPA.abs" #-}
div n d = (quot (I'.fromIntegral n) (I'.fromIntegral d))

mod :: _ => Int -> Int -> Int
{-# LINE 37 "..\pa\src\DisPA.abs" #-}
mod n d = (rem (I'.fromIntegral n) (I'.fromIntegral d))

diff :: _ => TimeSpec -> TimeSpec -> TimeSpec
{-# LINE 38 "..\pa\src\DisPA.abs" #-}
diff a b = (diffTimeSpec a b)

toVector :: _ => forall a . List a -> Vector a
{-# LINE 39 "..\pa\src\DisPA.abs" #-}
toVector l = (fromList l)

num :: _ => Int
{-# LINE 43 "..\pa\src\DisPA.abs" #-}
num = 10

d :: _ => Int
{-# LINE 44 "..\pa\src\DisPA.abs" #-}
d = 3

kinit :: _ => Int
{-# LINE 45 "..\pa\src\DisPA.abs" #-}
kinit = ((d) * ((d) + 1))

workers :: _ => Int
{-# LINE 46 "..\pa\src\DisPA.abs" #-}
workers = (numCapabilities)

isElem :: _ => forall a . a -> List a -> Bool
{-# LINE 49 "..\pa\src\DisPA.abs" #-}
isElem l ls
  = case ls of
        [] -> False
        (l_ : ls_) -> ((l == l_) || (isElem l ls_))

localIndex :: _ => Int -> Int
{-# LINE 53 "..\pa\src\DisPA.abs" #-}
localIndex index
  = (((div
         (((div ((I'.fromIntegral index) - 1) (d)) + 1) - ((d) + 2))
         (workers))
        * (d))
       + (mod ((I'.fromIntegral index) - 1) (d)))

actorIndex :: _ => Int -> Int
{-# LINE 54 "..\pa\src\DisPA.abs" #-}
actorIndex index
  = (mod (((div ((I'.fromIntegral index) - 1) (d)) + 1) - ((d) + 2))
       (workers))

{-# LINE 56 "..\pa\src\DisPA.abs" #-}
class IWorker' a where
        {-# LINE 57 "..\pa\src\DisPA.abs" #-}
        run_ :: Obj' a -> ABS' Unit
        
        {-# LINE 58 "..\pa\src\DisPA.abs" #-}
        init_ :: List IWorker -> Obj' a -> ABS' Unit
        
        {-# LINE 59 "..\pa\src\DisPA.abs" #-}
        request :: Int -> Obj' a -> ABS' Int

data IWorker = forall a . IWorker' a => IWorker (Obj' a)

instance I'.Show IWorker where
        show _ = "IWorker"

instance I'.Eq IWorker where
        IWorker (Obj' ref1' _) == IWorker (Obj' ref2' _)
          = ref1' == I'.unsafeCoerce ref2'

instance IWorker' Null' where
        run_ = I'.undefined
        init_ = I'.undefined
        request = I'.undefined

instance IWorker' a => Sub' (Obj' a) IWorker where
        up' = IWorker

{-# LINE 62 "..\pa\src\DisPA.abs" #-}
data Worker = Worker{ws2'Worker :: Vector IWorker,
                     workerId'Worker :: Int, size'Worker :: Int,
                     initArr'Worker :: IOVector (Fut Int), g'Worker :: GenIO,
                     arr'Worker :: IOVector (Fut Int), aliveDelegates'Worker :: Int}
smart'Worker workerId size
  = Worker{workerId'Worker = workerId, size'Worker = size,
           ws2'Worker = (I'.error "foreign object not initialized"),
           initArr'Worker = (I'.error "foreign object not initialized"),
           arr'Worker = (I'.error "foreign object not initialized"),
           g'Worker = (I'.error "foreign object not initialized"),
           aliveDelegates'Worker = 0}

init'Worker :: Obj' Worker -> I'.IO ()
{-# LINE 62 "..\pa\src\DisPA.abs" #-}
init'Worker this@(Obj' this' _)
  = do I'.writeIORef this' =<<
         ((\ this'' ->
             (\ v' -> this''{g'Worker = v'}) <$!> createSystemRandom)
            =<< I'.readIORef this')
       I'.writeIORef this' =<<
         ((\ this'' ->
             (\ v' -> this''{arr'Worker = v'}) <$!>
               (I'.join
                  ((I'.pure unsafeNew <*>
                      ((+) <$!> I'.pure (I'.fromIntegral (size'Worker this'')) <*>
                         I'.pure 1)))))
            =<< I'.readIORef this')
       I'.writeIORef this' =<<
         ((\ this'' ->
             (\ v' -> this''{initArr'Worker = v'}) <$!>
               (I'.join
                  ((I'.pure unsafeNew <*>
                      ((+) <$!> (I'.pure kinit) <*> I'.pure 1)))))
            =<< I'.readIORef this')

instance IWorker' Worker where
        run_ this@(Obj' this' _)
          = do j :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               i :: IORef' Int <- I'.liftIO
                                    ((\ this'' ->
                                        I'.newIORef
                                          (((d) + 2) +
                                             ((I'.fromIntegral (workerId'Worker this'')) - 1)))
                                       =<< I'.readIORef this')
               temp :: IORef' Int <- I'.liftIO
                                       ((\ this'' ->
                                           I'.newIORef
                                             ((kinit) +
                                                (((I'.fromIntegral (workerId'Worker this'')) - 1) *
                                                   d)))
                                          =<< I'.readIORef this')
               source :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               target :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               u :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               c :: IORef' (Fut Int) <- I'.liftIO (I'.newIORef nullFuture')
               while
                 ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> (I'.pure num))
                 (do I'.liftIO (I'.writeIORef j 1)
                     pastDraws :: IORef' (List Int) <- I'.liftIO (I'.newIORef [])
                     while
                       ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*> (I'.pure d))
                       (do I'.liftIO
                             (I'.writeIORef source =<<
                                (\ this'' ->
                                   (I'.join
                                      (((I'.pure uniformR <*>
                                           ((,) <$!> I'.pure 1 <*>
                                              ((*) <$!> (I'.fromIntegral <$!> I'.readIORef temp) <*>
                                                 I'.pure 2)))
                                          <*> I'.pure (g'Worker this'')))))
                                  =<< I'.readIORef this')
                           I'.liftIO
                             (I'.writeIORef target =<<
                                ((+) <$!> (I'.fromIntegral <$!> I'.readIORef temp) <*>
                                   (I'.fromIntegral <$!> I'.readIORef j)))
                           I'.liftIO (I'.writeIORef u 0)
                           if' <- I'.liftIO
                                    (((>) <$!> (I'.fromIntegral <$!> I'.readIORef source) <*>
                                        (I'.fromIntegral <$!> I'.readIORef temp)))
                           if if' then
                             do I'.liftIO
                                  (I'.writeIORef source =<<
                                     ((-) <$!> (I'.fromIntegral <$!> I'.readIORef source) <*>
                                        (I'.fromIntegral <$!> I'.readIORef temp)))
                                if' <- I'.liftIO
                                         (((>) <$!> (I'.fromIntegral <$!> I'.readIORef source) <*>
                                             (I'.pure kinit)))
                                if if' then
                                  do I'.liftIO
                                       (I'.writeIORef u =<<
                                          ((+) <$!>
                                             (I'.pure div <*>
                                                ((-) <$!> (I'.fromIntegral <$!> I'.readIORef source)
                                                   <*> I'.pure 1)
                                                <*> (I'.pure d))
                                             <*> I'.pure 1))
                                     if' <- I'.liftIO
                                              ((I'.pure isElem <*>
                                                  (I'.fromIntegral <$!> I'.readIORef u)
                                                  <*> I'.readIORef pastDraws))
                                     if if' then
                                       do I'.liftIO
                                            (I'.writeIORef j =<<
                                               ((-) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*>
                                                  I'.pure 1))
                                       else
                                       do I'.liftIO
                                            (I'.writeIORef pastDraws =<<
                                               ((:) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                                                  I'.readIORef pastDraws))
                                          lTarget :: IORef' Int <- I'.liftIO
                                                                     (I'.newIORef =<<
                                                                        (I'.pure localIndex <*>
                                                                           (I'.fromIntegral <$!>
                                                                              I'.readIORef target)))
                                          I'.liftIO
                                            (I'.writeIORef c =<<
                                               (\ this'' ->
                                                  (I'.join
                                                     (((I'.pure read <*>
                                                          I'.pure (arr'Worker this''))
                                                         <*>
                                                         (I'.fromIntegral <$!>
                                                            I'.readIORef lTarget)))))
                                                 =<< I'.readIORef this')
                                          I'.liftIO
                                            ((\ e1' ->
                                                pro_give e1' =<<
                                                  (I'.fromIntegral <$!> I'.readIORef u))
                                               =<< I'.readIORef c)
                                  else
                                  do I'.liftIO
                                       (I'.writeIORef c =<<
                                          (\ this'' ->
                                             (I'.join
                                                (((I'.pure read <*> I'.pure (initArr'Worker this''))
                                                    <*>
                                                    (I'.fromIntegral <$!> I'.readIORef source)))))
                                            =<< I'.readIORef this')
                                     I'.liftIO (I'.writeIORef u =<< (get =<< I'.readIORef c))
                                     if' <- I'.liftIO
                                              (((==) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                                                  (I'.negate <$!> I'.pure 1)))
                                     if if' then
                                       do I'.liftIO
                                            (I'.writeIORef j =<<
                                               ((-) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*>
                                                  I'.pure 1))
                                       else
                                       do I'.liftIO
                                            (I'.writeIORef u =<<
                                               ((+) <$!>
                                                  (I'.pure div <*>
                                                     ((-) <$!>
                                                        (I'.fromIntegral <$!> I'.readIORef source)
                                                        <*> I'.pure 1)
                                                     <*> (I'.pure d))
                                                  <*> I'.pure 1))
                                          if' <- I'.liftIO
                                                   ((I'.pure isElem <*>
                                                       (I'.fromIntegral <$!> I'.readIORef u)
                                                       <*> I'.readIORef pastDraws))
                                          if if' then
                                            do I'.liftIO
                                                 (I'.writeIORef j =<<
                                                    ((-) <$!> (I'.fromIntegral <$!> I'.readIORef j)
                                                       <*> I'.pure 1))
                                            else
                                            do I'.liftIO
                                                 (I'.writeIORef pastDraws =<<
                                                    ((:) <$!> (I'.fromIntegral <$!> I'.readIORef u)
                                                       <*> I'.readIORef pastDraws))
                                               lTarget :: IORef' Int <- I'.liftIO
                                                                          (I'.newIORef =<<
                                                                             (I'.pure localIndex <*>
                                                                                (I'.fromIntegral
                                                                                   <$!>
                                                                                   I'.readIORef
                                                                                     target)))
                                               I'.liftIO
                                                 (I'.writeIORef c =<<
                                                    (\ this'' ->
                                                       (I'.join
                                                          (((I'.pure read <*>
                                                               I'.pure (arr'Worker this''))
                                                              <*>
                                                              (I'.fromIntegral <$!>
                                                                 I'.readIORef lTarget)))))
                                                      =<< I'.readIORef this')
                                               I'.liftIO
                                                 ((\ e1' ->
                                                     pro_give e1' =<<
                                                       (I'.fromIntegral <$!> I'.readIORef u))
                                                    =<< I'.readIORef c)
                             else
                             do if' <- I'.liftIO
                                         (((>) <$!> (I'.fromIntegral <$!> I'.readIORef source) <*>
                                             (I'.pure kinit)))
                                if if' then
                                  do aIndex :: IORef' Int <- I'.liftIO
                                                               (I'.newIORef =<<
                                                                  (I'.pure actorIndex <*>
                                                                     (I'.fromIntegral <$!>
                                                                        I'.readIORef source)))
                                     w :: IORef' IWorker <- I'.liftIO
                                                              (I'.newIORef =<<
                                                                 (\ this'' ->
                                                                    (I'.join
                                                                       (((I'.pure indexM <*>
                                                                            I'.pure
                                                                              (ws2'Worker this''))
                                                                           <*>
                                                                           (I'.fromIntegral <$!>
                                                                              I'.readIORef
                                                                                aIndex)))))
                                                                   =<< I'.readIORef this')
                                     fp :: IORef' (Fut Int) <- I'.liftIO
                                                                 (I'.newIORef =<<
                                                                    ((\ (IWorker obj') ->
                                                                        (obj' <!>) =<<
                                                                          I'.pure request <*>
                                                                            (I'.fromIntegral <$!>
                                                                               I'.readIORef source))
                                                                       =<< I'.readIORef w))
                                     _ <- I'.liftIO
                                            ((this <!!>) =<<
                                               I'.pure delegate''Worker <*> I'.readIORef fp <*>
                                                 (I'.fromIntegral <$!> I'.readIORef target))
                                     I'.liftIO
                                       (I'.writeIORef this' =<<
                                          ((\ this'' ->
                                              this''{aliveDelegates'Worker =
                                                       ((I'.fromIntegral
                                                           (aliveDelegates'Worker this''))
                                                          + 1)})
                                             <$!> I'.readIORef this'))
                                  else
                                  do I'.liftIO
                                       (I'.writeIORef c =<<
                                          (\ this'' ->
                                             (I'.join
                                                (((I'.pure read <*> I'.pure (initArr'Worker this''))
                                                    <*>
                                                    (I'.fromIntegral <$!> I'.readIORef source)))))
                                            =<< I'.readIORef this')
                                     I'.liftIO (I'.writeIORef u =<< (get =<< I'.readIORef c))
                                     if' <- I'.liftIO
                                              (((==) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                                                  (I'.negate <$!> I'.pure 1)))
                                     if if' then
                                       do I'.liftIO
                                            (I'.writeIORef j =<<
                                               ((-) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*>
                                                  I'.pure 1))
                                       else
                                       do if' <- I'.liftIO
                                                   ((I'.pure isElem <*>
                                                       (I'.fromIntegral <$!> I'.readIORef u)
                                                       <*> I'.readIORef pastDraws))
                                          if if' then
                                            do I'.liftIO
                                                 (I'.writeIORef j =<<
                                                    ((-) <$!> (I'.fromIntegral <$!> I'.readIORef j)
                                                       <*> I'.pure 1))
                                            else
                                            do I'.liftIO
                                                 (I'.writeIORef pastDraws =<<
                                                    ((:) <$!> (I'.fromIntegral <$!> I'.readIORef u)
                                                       <*> I'.readIORef pastDraws))
                                               lTarget :: IORef' Int <- I'.liftIO
                                                                          (I'.newIORef =<<
                                                                             (I'.pure localIndex <*>
                                                                                (I'.fromIntegral
                                                                                   <$!>
                                                                                   I'.readIORef
                                                                                     target)))
                                               I'.liftIO
                                                 (I'.writeIORef c =<<
                                                    (\ this'' ->
                                                       (I'.join
                                                          (((I'.pure read <*>
                                                               I'.pure (arr'Worker this''))
                                                              <*>
                                                              (I'.fromIntegral <$!>
                                                                 I'.readIORef lTarget)))))
                                                      =<< I'.readIORef this')
                                               I'.liftIO
                                                 ((\ e1' ->
                                                     pro_give e1' =<<
                                                       (I'.fromIntegral <$!> I'.readIORef u))
                                                    =<< I'.readIORef c)
                           I'.liftIO
                             (I'.writeIORef j =<<
                                ((+) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*> I'.pure 1)))
                     I'.liftIO
                       (I'.writeIORef i =<<
                          ((+) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*>
                             (I'.pure workers)))
                     I'.liftIO
                       (I'.writeIORef temp =<<
                          ((+) <$!> (I'.fromIntegral <$!> I'.readIORef temp) <*>
                             ((*) <$!> (I'.pure d) <*> (I'.pure workers)))))
               awaitBool' this
                 (\ this'' ->
                    ((I'.fromIntegral (aliveDelegates'Worker this'')) == 0))
        init_ ws this@(Obj' this' _)
          = do I'.liftIO
                 (I'.writeIORef this' =<<
                    ((\ this'' -> this''{ws2'Worker = (toVector ws)}) <$!>
                       I'.readIORef this'))
               c :: IORef' (Fut Int) <- I'.liftIO (I'.newIORef nullFuture')
               index :: IORef' Int <- I'.liftIO (I'.newIORef 1)
               i :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               j :: IORef' Int <- I'.liftIO (I'.newIORef 0)
               while
                 ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> (I'.pure d))
                 (do I'.liftIO (I'.writeIORef j 1)
                     while
                       ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*> (I'.pure d))
                       (do I'.liftIO (I'.writeIORef c =<< pro_new)
                           I'.liftIO
                             (I'.writeIORef index =<<
                                ((+) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*>
                                   ((*) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*>
                                      (I'.pure d))))
                           if' <- I'.liftIO
                                    (((<=) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*>
                                        (I'.fromIntegral <$!> I'.readIORef i)))
                           if if' then
                             do I'.liftIO
                                  ((\ e1' -> pro_give e1' =<< (I'.fromIntegral <$!> I'.readIORef j))
                                     =<< I'.readIORef c)
                             else
                             do I'.liftIO
                                  ((\ e1' -> pro_give e1' =<< (I'.negate <$!> I'.pure 1)) =<<
                                     I'.readIORef c)
                           _ <- I'.liftIO
                                  ((\ this'' ->
                                      (I'.join
                                         ((((I'.pure write <*> I'.pure (initArr'Worker this'')) <*>
                                              (I'.fromIntegral <$!> I'.readIORef index))
                                             <*> I'.readIORef c))))
                                     =<< I'.readIORef this')
                           I'.liftIO
                             (I'.writeIORef j =<<
                                ((+) <$!> (I'.fromIntegral <$!> I'.readIORef j) <*> I'.pure 1)))
                     I'.liftIO
                       (I'.writeIORef i =<<
                          ((+) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 1)))
               I'.liftIO (I'.writeIORef index 0)
               while
                 ((\ this'' ->
                     ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef index) <*>
                        I'.pure (I'.fromIntegral (size'Worker this''))))
                    =<< I'.readIORef this')
                 (do I'.liftIO (I'.writeIORef c =<< pro_new)
                     _ <- I'.liftIO
                            ((\ this'' ->
                                (I'.join
                                   ((((I'.pure write <*> I'.pure (arr'Worker this'')) <*>
                                        (I'.fromIntegral <$!> I'.readIORef index))
                                       <*> I'.readIORef c))))
                               =<< I'.readIORef this')
                     I'.liftIO
                       (I'.writeIORef index =<<
                          ((+) <$!> (I'.fromIntegral <$!> I'.readIORef index) <*>
                             I'.pure 1)))
        request source this@(Obj' this' _)
          = do c :: IORef' (Fut Int) <- I'.liftIO (I'.newIORef nullFuture')
               if ((I'.fromIntegral source) > (kinit)) then
                 do lSource :: IORef' Int <- I'.liftIO
                                               (I'.newIORef (localIndex (I'.fromIntegral source)))
                    I'.liftIO
                      (I'.writeIORef c =<<
                         (\ this'' ->
                            (I'.join
                               (((I'.pure read <*> I'.pure (arr'Worker this'')) <*>
                                   (I'.fromIntegral <$!> I'.readIORef lSource)))))
                           =<< I'.readIORef this')
                    awaitFuture' this =<< I'.liftIO (I'.readIORef c)
                 else
                 do I'.liftIO
                      (I'.writeIORef c =<<
                         (\ this'' ->
                            (I'.join
                               (((I'.pure read <*> I'.pure (initArr'Worker this'')) <*>
                                   I'.pure (I'.fromIntegral source)))))
                           =<< I'.readIORef this')
               I'.liftIO (get =<< I'.readIORef c)

delegate''Worker :: Fut Int -> Int -> Obj' Worker -> ABS' Unit
delegate''Worker ft target this@(Obj' this' _)
  = do awaitFuture' this ft
       u :: IORef' Int <- I'.liftIO (I'.newIORef =<< get ft)
       c :: IORef' (Fut Int) <- I'.liftIO (I'.newIORef nullFuture')
       found :: IORef' Bool <- I'.liftIO (I'.newIORef False)
       when' <- I'.liftIO
                  (((not) <$!>
                      ((==) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                         (I'.negate <$!> I'.pure 1))))
       I'.when when'
         (do i :: IORef' Int <- I'.liftIO
                                  (I'.newIORef
                                     (((div ((I'.fromIntegral target) - 1) (d)) * (d)) + 1))
             lCurrentNode :: IORef' Int <- I'.liftIO
                                             (I'.newIORef =<<
                                                ((-) <$!>
                                                   ((+) <$!> (I'.fromIntegral <$!> I'.readIORef i)
                                                      <*> (I'.pure d))
                                                   <*> I'.pure 1))
             item :: IORef' Int <- I'.liftIO (I'.newIORef 0)
             maybeElement :: IORef' (Maybe Int) <- I'.liftIO (I'.newIORef Nothing)
             v :: IORef' Int <- I'.liftIO (I'.newIORef 0)
             while
               ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*>
                  (I'.fromIntegral <$!> I'.readIORef lCurrentNode))
               (do li :: IORef' Int <- I'.liftIO
                                         (I'.newIORef =<<
                                            (I'.pure localIndex <*>
                                               (I'.fromIntegral <$!> I'.readIORef i)))
                   I'.liftIO
                     (I'.writeIORef c =<<
                        (\ this'' ->
                           (I'.join
                              (((I'.pure read <*> I'.pure (arr'Worker this'')) <*>
                                  (I'.fromIntegral <$!> I'.readIORef li)))))
                          =<< I'.readIORef this')
                   I'.liftIO
                     (I'.writeIORef maybeElement =<< (pro_try =<< I'.readIORef c))
                   when' <- I'.liftIO ((I'.pure isJust <*> I'.readIORef maybeElement))
                   I'.when when'
                     (do I'.liftIO
                           (I'.writeIORef v =<<
                              (I'.pure fromJust <*> I'.readIORef maybeElement))
                         when' <- I'.liftIO
                                    (((==) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                                        (I'.fromIntegral <$!> I'.readIORef v)))
                         I'.when when'
                           (do I'.liftIO (I'.writeIORef found True)
                               I'.liftIO
                                 (I'.writeIORef i =<<
                                    ((+) <$!> (I'.fromIntegral <$!> I'.readIORef lCurrentNode) <*>
                                       I'.pure 1))))
                   I'.liftIO
                     (I'.writeIORef i =<<
                        ((+) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 1))))
       if' <- I'.liftIO
                (((||) <$!> I'.readIORef found <*>
                    ((==) <$!> (I'.fromIntegral <$!> I'.readIORef u) <*>
                       (I'.negate <$!> I'.pure 1))))
       if if' then
         do I'.liftIO
              (I'.writeIORef u ((div ((I'.fromIntegral target) - 1) (d)) * (d)))
            u2 :: IORef' Int <- I'.liftIO (I'.newIORef 0)
            while
              ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef u2) <*>
                 I'.pure kinit)
              (do I'.liftIO
                    (I'.writeIORef u2 =<<
                       (\ this'' ->
                          (I'.join
                             (((I'.pure uniformR <*>
                                  ((,) <$!> I'.pure 1 <*> (I'.fromIntegral <$!> I'.readIORef u)))
                                 <*> I'.pure (g'Worker this'')))))
                         =<< I'.readIORef this'))
            aIndex :: IORef' Int <- I'.liftIO
                                      (I'.newIORef =<<
                                         (I'.pure actorIndex <*>
                                            (I'.fromIntegral <$!> I'.readIORef u2)))
            w :: IORef' IWorker <- I'.liftIO
                                     (I'.newIORef =<<
                                        (\ this'' ->
                                           (I'.join
                                              (((I'.pure indexM <*> I'.pure (ws2'Worker this'')) <*>
                                                  (I'.fromIntegral <$!> I'.readIORef aIndex)))))
                                          =<< I'.readIORef this')
            fp :: IORef' (Fut Int) <- I'.liftIO
                                        (I'.newIORef =<<
                                           ((\ (IWorker obj') ->
                                               (obj' <!>) =<<
                                                 I'.pure request <*>
                                                   (I'.fromIntegral <$!> I'.readIORef u2))
                                              =<< I'.readIORef w))
            _ <- (this <..>) =<<
                   I'.liftIO
                     (I'.pure delegate''Worker <*> I'.readIORef fp <*>
                        I'.pure (I'.fromIntegral target))
            I'.pure ()
         else
         do lTarget :: IORef' Int <- I'.liftIO
                                       (I'.newIORef (localIndex (I'.fromIntegral target)))
            I'.liftIO
              (I'.writeIORef c =<<
                 (\ this'' ->
                    (I'.join
                       (((I'.pure read <*> I'.pure (arr'Worker this'')) <*>
                           (I'.fromIntegral <$!> I'.readIORef lTarget)))))
                   =<< I'.readIORef this')
            I'.liftIO
              ((\ e1' -> pro_give e1' =<< (I'.fromIntegral <$!> I'.readIORef u))
                 =<< I'.readIORef c)
            I'.liftIO
              (I'.writeIORef this' =<<
                 ((\ this'' ->
                     this''{aliveDelegates'Worker =
                              ((I'.fromIntegral (aliveDelegates'Worker this'')) - 1)})
                    <$!> I'.readIORef this'))
main
  = main_is'
      (\ this ->
         do i :: IORef' Int <- I'.liftIO (I'.newIORef (workers))
            w :: IORef' IWorker <- I'.liftIO (I'.newIORef (IWorker null))
            size :: IORef' Int <- I'.liftIO
                                    (I'.newIORef (div ((num) - ((d) + 1)) (workers)))
            off :: IORef' Int <- I'.liftIO
                                   (I'.newIORef =<<
                                      ((-) <$!>
                                         ((-) <$!> (I'.pure num) <*>
                                            ((+) <$!> (I'.pure d) <*> I'.pure 1))
                                         <*>
                                         ((*) <$!> (I'.fromIntegral <$!> I'.readIORef size) <*>
                                            (I'.pure workers))))
            ws :: IORef' (List IWorker) <- I'.liftIO (I'.newIORef [])
            while
              ((>) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 0)
              (do if' <- I'.liftIO
                           (((>=) <$!> (I'.fromIntegral <$!> I'.readIORef off) <*>
                               (I'.fromIntegral <$!> I'.readIORef i)))
                  if if' then
                    do I'.lift ((I'.liftIO . (I'.writeIORef w . IWorker)) =<<
                            (new init'Worker =<<
                               I'.pure smart'Worker <*> (I'.fromIntegral <$!> I'.liftIO (I'.readIORef i)) <*>
                                 ((*) <$!>
                                    ((+) <$!> (I'.fromIntegral <$!> I'.liftIO (I'.readIORef size)) <*>
                                       I'.pure 1)
                                    <*> (I'.pure d))))
                    else
                    do I'.lift
                         (I'.liftIO . (I'.writeIORef w . IWorker) =<<
                            (new init'Worker =<<
                               I'.pure smart'Worker <*> (I'.fromIntegral <$!> I'.liftIO (I'.readIORef i)) <*>
                                 ((*) <$!> (I'.fromIntegral <$!> I'.liftIO (I'.readIORef size)) <*>
                                    (I'.pure d))))
                  I'.liftIO
                    (I'.writeIORef ws =<<
                       ((:) <$!> (up' <$!> I'.readIORef w) <*> I'.readIORef ws))
                  I'.liftIO
                    (I'.writeIORef i =<<
                       ((-) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 1)))
            ff :: IORef' (Fut Unit) <- I'.liftIO (I'.newIORef nullFuture')
            ws_ :: IORef' (List IWorker) <- I'.liftIO
                                              (I'.newIORef =<< I'.readIORef ws)
            fs :: IORef' (List (Fut Unit)) <- I'.liftIO (I'.newIORef [])
            while ((not) <$!> ((==) <$!> I'.readIORef ws_ <*> I'.pure []))
              (do I'.liftIO
                    (I'.writeIORef w =<< (I'.pure head <*> I'.readIORef ws_))
                  I'.liftIO
                    (I'.writeIORef ff =<<
                       ((\ (IWorker obj') ->
                           (obj' <!>) =<< I'.pure init_ <*> I'.readIORef ws)
                          =<< I'.readIORef w))
                  I'.liftIO
                    (I'.writeIORef fs =<<
                       ((:) <$!> I'.readIORef ff <*> I'.readIORef fs))
                  I'.liftIO
                    (I'.writeIORef ws_ =<< (I'.pure tail <*> I'.readIORef ws_)))
            while ((not) <$!> ((==) <$!> I'.readIORef fs <*> I'.pure []))
              (do I'.liftIO
                    (I'.writeIORef ff =<< (I'.pure head <*> I'.readIORef fs))
                  _ <- I'.liftIO (get =<< I'.readIORef ff)
                  I'.liftIO (I'.writeIORef fs =<< (I'.pure tail <*> I'.readIORef fs)))
            I'.liftIO (println "START RUNNING")
            t1 :: IORef' TimeSpec <- I'.liftIO
                                       (I'.newIORef =<<
                                          (I'.join ((I'.pure getTime <*> I'.pure Monotonic))))
            while ((not) <$!> ((==) <$!> I'.readIORef ws <*> I'.pure []))
              (do I'.liftIO
                    (I'.writeIORef w =<< (I'.pure head <*> I'.readIORef ws))
                  I'.liftIO
                    (I'.writeIORef ff =<<
                       ((\ (IWorker obj') -> (obj' <!> run_)) =<< I'.readIORef w))
                  I'.liftIO
                    (I'.writeIORef fs =<<
                       ((:) <$!> I'.readIORef ff <*> I'.readIORef fs))
                  I'.liftIO (I'.writeIORef ws =<< (I'.pure tail <*> I'.readIORef ws)))
            while ((not) <$!> ((==) <$!> I'.readIORef fs <*> I'.pure []))
              (do I'.liftIO
                    (I'.writeIORef ff =<< (I'.pure head <*> I'.readIORef fs))
                  _ <- I'.liftIO (get =<< I'.readIORef ff)
                  I'.liftIO (I'.writeIORef fs =<< (I'.pure tail <*> I'.readIORef fs)))
            I'.liftIO
              (println =<<
                 (I'.pure toString <*>
                    (I'.pure diff <*>
                       (I'.join ((I'.pure getTime <*> I'.pure Monotonic)))
                       <*> I'.readIORef t1))))
