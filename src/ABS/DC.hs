{-# LINE 1 "..\habs-runtime\src\ABS\DC.abs" #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module ABS.DC (main, module ABS.DC) where
import ABS.Runtime
import ABS.StdLib
import Data.Function ((.))
import Control.Applicative ((<*>), (*>))
import Control.Monad ((=<<))
import qualified Control.Applicative as I' (pure)
import qualified Data.IORef as I' (newIORef, readIORef, writeIORef)
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

default (Int, Rat)

{-# LINE 5 "..\habs-runtime\src\ABS\DC.abs" #-}
data Resourcetype = Speed
                  | Cores
                  | Bandwidth
                  | Memory
                  | Startupduration
                  | Shutdownduration
                  | PaymentInterval
                  | CostPerInterval
                  deriving (I'.Eq, I'.Show)

{-# LINE 15 "..\habs-runtime\src\ABS\DC.abs" #-}
class DC' a where
        {-# LINE 18 "..\habs-runtime\src\ABS\DC.abs" #-}
        load :: Resourcetype -> Int -> Obj' a -> ABS' Rat
        
        {-# LINE 20 "..\habs-runtime\src\ABS\DC.abs" #-}
        total :: Resourcetype -> Obj' a -> ABS' Rat
        
        {-# LINE 23 "..\habs-runtime\src\ABS\DC.abs" #-}
        request__ :: Int -> Obj' a -> ABS' Unit

data DC = forall a . DC' a => DC (Obj' a)

instance I'.Show DC where
        show _ = "DC"

instance I'.Eq DC where
        DC (Obj' ref1' _) == DC (Obj' ref2' _)
          = ref1' == I'.unsafeCoerce ref2'

instance DC' Null' where
        load = I'.undefined
        total = I'.undefined
        request__ = I'.undefined

instance DC' a => Sub' (Obj' a) DC where
        up' = DC

{-# LINE 26 "..\habs-runtime\src\ABS\DC.abs" #-}
data SimDC = SimDC{instrPS'SimDC :: Int}
smart'SimDC instrPS = SimDC{instrPS'SimDC = instrPS}

init'SimDC :: Obj' SimDC -> I'.IO ()
{-# LINE 26 "..\habs-runtime\src\ABS\DC.abs" #-}
init'SimDC this@(Obj' this' _) = I'.pure ()

instance DC' SimDC where
        load rtype periods this@(Obj' this' _) = do I'.lift (I'.pure 0)
        total rtype this@(Obj' this' _) = do I'.lift (I'.pure 0)
        request__ nrInstr this@(Obj' this' _)
          = do I'.lift (println (toString (I'.fromIntegral nrInstr)))
               (\ this'' ->
                  if
                    ((I'.fromIntegral nrInstr) >
                       (I'.fromIntegral (instrPS'SimDC this'')))
                    then
                    do I'.lift (duration 1 1)
                       suspend this
                       (\ this'' ->
                          this <..>
                            request__
                              ((I'.fromIntegral nrInstr) -
                                 (I'.fromIntegral (instrPS'SimDC this''))))
                         =<< I'.lift (I'.readIORef this')
                    else
                    do remaining :: IORef' Rat <- I'.lift
                                                    ((\ this'' ->
                                                        I'.newIORef
                                                          ((I'.fromIntegral nrInstr) /
                                                             (I'.fromIntegral
                                                                (instrPS'SimDC this''))))
                                                       =<< I'.readIORef this')
                       I'.lift
                         ((\ e1' -> duration e1' =<< I'.readIORef remaining) =<<
                            I'.readIORef remaining))
                 =<< I'.lift (I'.readIORef this')
main
  = main_is'
      (\ this ->
         do o :: IORef' DC <- I'.lift
                                (((I'.newIORef . DC) =<< new init'SimDC (smart'SimDC 3)))
            f1 :: IORef' (Fut Unit) <- I'.lift
                                         (I'.newIORef =<<
                                            ((\ (DC obj') -> (obj' <!> request__ 10)) =<<
                                               I'.readIORef o))
            f2 :: IORef' (Fut Unit) <- I'.lift
                                         (I'.newIORef =<<
                                            ((\ (DC obj') -> (obj' <!> request__ 19)) =<<
                                               I'.readIORef o))
            awaitFutures' this
              [(I'.isEmptyMVar =<< I'.readIORef f1),
               (I'.isEmptyMVar =<< I'.readIORef f2)]
              ((I'.readMVar =<< I'.readIORef f1) *>
                 (I'.readMVar =<< I'.readIORef f2)))
