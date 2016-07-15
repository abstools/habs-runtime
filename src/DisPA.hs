{-# LINE 1 "..\pa\src\DisPA.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module DisPA (main) where
import ABS.Runtime
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
       (isEmptyMVar, readMVar)
import Control.Exception (assert)
import qualified Control.Exception as I'
       (Exception(..), SomeException, throwTo, throw)
{-# LINE 4 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (IOVector(..))
{-# LINE 5 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (unsafeNew)
{-# LINE 6 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (unsafeRead)
{-# LINE 7 "..\pa\src\DisPA.abs" #-}
import Data.Vector.Mutable (unsafeWrite)
{-# LINE 10 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (GenIO(..))
{-# LINE 11 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (createSystemRandom)
{-# LINE 12 "..\pa\src\DisPA.abs" #-}
import System.Random.MWC (uniformR)
{-# LINE 15 "..\pa\src\DisPA.abs" #-}
import System.Clock (getTime)
{-# LINE 16 "..\pa\src\DisPA.abs" #-}
import System.Clock (diffTimeSpec)
{-# LINE 17 "..\pa\src\DisPA.abs" #-}
import System.Clock (Clock(..))
{-# LINE 18 "..\pa\src\DisPA.abs" #-}
import System.Clock (TimeSpec(..))
{-# LINE 21 "..\pa\src\DisPA.abs" #-}
import GHC.Conc (numCapabilities)
{-# LINE 24 "..\pa\src\DisPA.abs" #-}
import System.IO.Unsafe (unsafePerformIO)
{-# LINE 27 "..\pa\src\DisPA.abs" #-}
import Prelude (quot)
{-# LINE 28 "..\pa\src\DisPA.abs" #-}
import Prelude (rem)

default (Int, Rat)

div :: _ => Int -> Int -> Int
{-# LINE 31 "..\pa\src\DisPA.abs" #-}
div n d = (quot (I'.fromIntegral n) (I'.fromIntegral d))

mod :: _ => Int -> Int -> Int
{-# LINE 32 "..\pa\src\DisPA.abs" #-}
mod n d = (rem (I'.fromIntegral n) (I'.fromIntegral d))

diff :: _ => TimeSpec -> TimeSpec -> TimeSpec
{-# LINE 33 "..\pa\src\DisPA.abs" #-}
diff a b = (diffTimeSpec a b)

num :: _ => Int
{-# LINE 38 "..\pa\src\DisPA.abs" #-}
num = 1000

d :: _ => Int
{-# LINE 39 "..\pa\src\DisPA.abs" #-}
d = 3

kinit :: _ => Int
{-# LINE 40 "..\pa\src\DisPA.abs" #-}
kinit = ((d) * ((d) + 1))

workers :: _ => Int
{-# LINE 41 "..\pa\src\DisPA.abs" #-}
workers = (numCapabilities)

isElem :: _ => forall a . a -> List a -> Bool
{-# LINE 44 "..\pa\src\DisPA.abs" #-}
isElem l ls
  = case ls of
        [] -> False
        (l_ : ls_) -> ((l == l_) || (isElem l ls_))

localIndex :: _ => Int -> Int
{-# LINE 48 "..\pa\src\DisPA.abs" #-}
localIndex index
  = (((div
         (((div ((I'.fromIntegral index) - 1) (d)) + 1) - ((d) + 2))
         (workers))
        * (d))
       + (mod ((I'.fromIntegral index) - 1) (d)))

actorIndex :: _ => Int -> Int
{-# LINE 49 "..\pa\src\DisPA.abs" #-}
actorIndex index
  = (mod (((div ((I'.fromIntegral index) - 1) (d)) + 1) - ((d) + 2))
       (workers))

ws2 :: _ => IOVector IWorker
{-# LINE 52 "..\pa\src\DisPA.abs" #-}
ws2 = (unsafePerformIO (unsafeNew ((workers) + 1)))

{-# LINE 54 "..\pa\src\DisPA.abs" #-}
class IWorker' a where
        {-# LINE 55 "..\pa\src\DisPA.abs" #-}
        run_ :: Obj' a -> ABS' Unit
        
        {-# LINE 56 "..\pa\src\DisPA.abs" #-}
        init_ :: Obj' a -> ABS' Unit
        
        {-# LINE 57 "..\pa\src\DisPA.abs" #-}
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

{-# LINE 60 "..\pa\src\DisPA.abs" #-}
data Worker = Worker{workerId'Worker :: Int, size'Worker :: Int,
                     initArr'Worker :: IOVector (Fut Int), g'Worker :: GenIO,
                     arr'Worker :: IOVector (Fut Int), aliveDelegates'Worker :: Int}
smart'Worker workerId size
  = Worker{workerId'Worker = workerId, size'Worker = size,
           initArr'Worker = (I'.error "foreign object not initialized"),
           arr'Worker = (I'.error "foreign object not initialized"),
           g'Worker = (I'.error "foreign object not initialized"),
           aliveDelegates'Worker = 0}

init'Worker :: Obj' Worker -> I'.IO ()
{-# LINE 60 "..\pa\src\DisPA.abs" #-}
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
                                                     (((I'.pure unsafeRead <*>
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
                                                (((I'.pure unsafeRead <*>
                                                     I'.pure (initArr'Worker this''))
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
                                                          (((I'.pure unsafeRead <*>
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
                                     when' <- I'.liftIO
                                                (((<) <$!>
                                                    (I'.fromIntegral <$!> I'.readIORef aIndex)
                                                    <*> I'.pure 0))
                                     I'.when when' (do I'.liftIO (println "HERE!"))
                                     w :: IORef' IWorker <- I'.liftIO
                                                              (I'.newIORef =<<
                                                                 (I'.join
                                                                    (((I'.pure unsafeRead <*>
                                                                         (I'.pure ws2))
                                                                        <*>
                                                                        (I'.fromIntegral <$!>
                                                                           I'.readIORef aIndex)))))
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
                                                (((I'.pure unsafeRead <*>
                                                     I'.pure (initArr'Worker this''))
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
                                                          (((I'.pure unsafeRead <*>
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
        init_ this@(Obj' this' _)
          = do c :: IORef' (Fut Int) <- I'.liftIO (I'.newIORef nullFuture')
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
                                         ((((I'.pure unsafeWrite <*>
                                               I'.pure (initArr'Worker this''))
                                              <*> (I'.fromIntegral <$!> I'.readIORef index))
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
                                   ((((I'.pure unsafeWrite <*> I'.pure (arr'Worker this'')) <*>
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
                               (((I'.pure unsafeRead <*> I'.pure (arr'Worker this'')) <*>
                                   (I'.fromIntegral <$!> I'.readIORef lSource)))))
                           =<< I'.readIORef this')
                    awaitFuture' this =<< I'.liftIO (I'.readIORef c)
                 else
                 do I'.liftIO
                      (I'.writeIORef c =<<
                         (\ this'' ->
                            (I'.join
                               (((I'.pure unsafeRead <*> I'.pure (initArr'Worker this'')) <*>
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
                              (((I'.pure unsafeRead <*> I'.pure (arr'Worker this'')) <*>
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
            I'.liftIO
              (I'.writeIORef u =<<
                 (\ this'' ->
                    (I'.join
                       (((I'.pure uniformR <*>
                            ((,) <$!> I'.pure 1 <*> (I'.fromIntegral <$!> I'.readIORef u)))
                           <*> I'.pure (g'Worker this'')))))
                   =<< I'.readIORef this')
            aIndex :: IORef' Int <- I'.liftIO
                                      (I'.newIORef =<<
                                         (I'.pure actorIndex <*>
                                            (I'.fromIntegral <$!> I'.readIORef u)))
            w :: IORef' IWorker <- I'.liftIO
                                     (I'.newIORef =<<
                                        (I'.join
                                           (((I'.pure unsafeRead <*> (I'.pure ws2)) <*>
                                               (I'.fromIntegral <$!> I'.readIORef aIndex)))))
            fp :: IORef' (Fut Int) <- I'.liftIO
                                        (I'.newIORef =<<
                                           ((\ (IWorker obj') ->
                                               (obj' <!>) =<<
                                                 I'.pure request <*>
                                                   (I'.fromIntegral <$!> I'.readIORef u))
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
                       (((I'.pure unsafeRead <*> I'.pure (arr'Worker this'')) <*>
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
         do i :: IORef' Int <- I'.liftIO (I'.newIORef 1)
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
              ((<=) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*>
                 (I'.pure workers))
              (do if' <- I'.liftIO
                           (((>=) <$!> (I'.fromIntegral <$!> I'.readIORef off) <*>
                               (I'.fromIntegral <$!> I'.readIORef i)))
                  if if' then
                    do I'.lift
                         ((I'.liftIO . I'.writeIORef w . IWorker) =<<
                            (new init'Worker =<< I'.liftIO (
                               I'.pure smart'Worker <*> (I'.fromIntegral <$!> I'.readIORef i) <*>
                                 ((*) <$!>
                                    ((+) <$!> (I'.fromIntegral <$!> I'.readIORef size) <*>
                                       I'.pure 1)
                                    <*> (I'.pure d)))))
                    else
                    do I'.lift
                         ((I'.liftIO . I'.writeIORef w . IWorker) =<<
                            (new init'Worker =<< I'.liftIO (
                               I'.pure smart'Worker <*> (I'.fromIntegral <$!> I'.readIORef i) <*>
                                 ((*) <$!> (I'.fromIntegral <$!> I'.readIORef size) <*>
                                    (I'.pure d)))))
                  I'.liftIO
                    (I'.writeIORef ws =<<
                       ((:) <$!> (up' <$!> I'.readIORef w) <*> I'.readIORef ws))
                  _ <- I'.liftIO
                         ((I'.join
                             ((((I'.pure unsafeWrite <*> (I'.pure ws2)) <*>
                                  ((-) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 1))
                                 <*> (up' <$!> I'.readIORef w)))))
                  I'.liftIO
                    (I'.writeIORef i =<<
                       ((+) <$!> (I'.fromIntegral <$!> I'.readIORef i) <*> I'.pure 1)))
            ff :: IORef' (Fut Unit) <- I'.liftIO (I'.newIORef nullFuture')
            ws_ :: IORef' (List IWorker) <- I'.liftIO
                                              (I'.newIORef =<< I'.readIORef ws)
            fs :: IORef' (List (Fut Unit)) <- I'.liftIO (I'.newIORef [])
            while ((not) <$!> ((==) <$!> I'.readIORef ws_ <*> I'.pure []))
              (do I'.liftIO
                    (I'.writeIORef w =<< (I'.pure head <*> I'.readIORef ws_))
                  I'.liftIO
                    (I'.writeIORef ff =<<
                       ((\ (IWorker obj') -> (obj' <!> init_)) =<< I'.readIORef w))
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
