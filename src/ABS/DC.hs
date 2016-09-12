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

{-# LINE 11 "..\habs-runtime\src\ABS\DC.abs" #-}
data InfRat = InfRat_
            | Fin !Rat
            deriving (I'.Eq, I'.Show)
finvalue (Fin a) = a
finvalue _
  = I'.throw
      (RecSelError
         (concatenate "Data constructor does not have accessor "
            "finvalue"))

{-# LINE 13 "..\habs-runtime\src\ABS\DC.abs" #-}
data Resourcetype = Speed
                  | Cores
                  | Bandwidth
                  | Memory
                  | Startupduration
                  | Shutdownduration
                  | PaymentInterval
                  | CostPerInterval
                  deriving (I'.Eq, I'.Show, I'.Ord)

{-# LINE 23 "..\habs-runtime\src\ABS\DC.abs" #-}
class DeploymentComponent' a where
        {-# LINE 24 "..\habs-runtime\src\ABS\DC.abs" #-}
        load :: Resourcetype -> Int -> Obj' a -> ABS' Rat
        
        {-# LINE 25 "..\habs-runtime\src\ABS\DC.abs" #-}
        total :: Resourcetype -> Obj' a -> ABS' InfRat
        
        {-# LINE 26 "..\habs-runtime\src\ABS\DC.abs" #-}
        transfer ::
                 DeploymentComponent -> Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        {-# LINE 27 "..\habs-runtime\src\ABS\DC.abs" #-}
        decrementResources :: Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        {-# LINE 28 "..\habs-runtime\src\ABS\DC.abs" #-}
        incrementResources :: Rat -> Resourcetype -> Obj' a -> ABS' Unit
        
        {-# LINE 29 "..\habs-runtime\src\ABS\DC.abs" #-}
        getName :: Obj' a -> ABS' String
        
        {-# LINE 31 "..\habs-runtime\src\ABS\DC.abs" #-}
        getCreationTime :: Obj' a -> ABS' Time
        
        {-# LINE 32 "..\habs-runtime\src\ABS\DC.abs" #-}
        getStartupDuration :: Obj' a -> ABS' Rat
        
        {-# LINE 33 "..\habs-runtime\src\ABS\DC.abs" #-}
        getShutdownDuration :: Obj' a -> ABS' Rat
        
        {-# LINE 34 "..\habs-runtime\src\ABS\DC.abs" #-}
        getPaymentInterval :: Obj' a -> ABS' Int
        
        {-# LINE 35 "..\habs-runtime\src\ABS\DC.abs" #-}
        getCostPerInterval :: Obj' a -> ABS' Rat
        
        {-# LINE 36 "..\habs-runtime\src\ABS\DC.abs" #-}
        getNumberOfCores :: Obj' a -> ABS' Rat
        
        {-# LINE 37 "..\habs-runtime\src\ABS\DC.abs" #-}
        acquire :: Obj' a -> ABS' Bool
        
        {-# LINE 38 "..\habs-runtime\src\ABS\DC.abs" #-}
        release :: Obj' a -> ABS' Bool
        
        {-# LINE 39 "..\habs-runtime\src\ABS\DC.abs" #-}
        shutdown :: Obj' a -> ABS' Bool
        
        {-# LINE 42 "..\habs-runtime\src\ABS\DC.abs" #-}
        request__ :: Int -> Obj' a -> ABS' Unit

data DeploymentComponent = forall a . DeploymentComponent' a =>
                             DeploymentComponent (Obj' a)

instance I'.Show DeploymentComponent where
        show _ = "DeploymentComponent"

instance I'.Eq DeploymentComponent where
        DeploymentComponent (Obj' ref1' _ _) ==
          DeploymentComponent (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance DeploymentComponent' Null' where
        load = I'.undefined
        total = I'.undefined
        transfer = I'.undefined
        decrementResources = I'.undefined
        incrementResources = I'.undefined
        getName = I'.undefined
        getCreationTime = I'.undefined
        getStartupDuration = I'.undefined
        getShutdownDuration = I'.undefined
        getPaymentInterval = I'.undefined
        getCostPerInterval = I'.undefined
        getNumberOfCores = I'.undefined
        acquire = I'.undefined
        release = I'.undefined
        shutdown = I'.undefined
        request__ = I'.undefined

instance DeploymentComponent' a => Sub' (Obj' a)
         DeploymentComponent where
        up' = DeploymentComponent

{-# LINE 46 "..\habs-runtime\src\ABS\DC.abs" #-}
data MainDeploymentComponent = MainDeploymentComponent{}

smart'MainDeploymentComponent :: MainDeploymentComponent
smart'MainDeploymentComponent = (MainDeploymentComponent)

init'MainDeploymentComponent ::
                             Obj' MainDeploymentComponent -> I'.IO ()
{-# LINE 46 "..\habs-runtime\src\ABS\DC.abs" #-}
init'MainDeploymentComponent this@(Obj' this' _ thisDC)
  = I'.pure ()

instance DeploymentComponent' MainDeploymentComponent where
        load rtype periods this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        total rtype this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure (Fin 0))
        transfer target amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        decrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        incrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        getName this@(Obj' this' _ thisDC) = do I'.lift (I'.pure "<main>")
        getCreationTime this@(Obj' this' _ thisDC) = do I'.lift (now)
        getStartupDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getShutdownDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getPaymentInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getCostPerInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getNumberOfCores this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        acquire this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        release this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        shutdown this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        request__ nrInstr this@(Obj' this' _ thisDC) = I'.pure ()

{-# LINE 110 "..\habs-runtime\src\ABS\DC.abs" #-}
data SimDeploymentComponent = SimDeploymentComponent{description'SimDeploymentComponent
                                                     :: String,
                                                     initconfig'SimDeploymentComponent ::
                                                     Map Resourcetype Rat,
                                                     instrPS'SimDeploymentComponent :: Int}

smart'SimDeploymentComponent ::
                             String -> Map Resourcetype Rat -> SimDeploymentComponent
smart'SimDeploymentComponent description'this initconfig'this
  = (\ instrPS'this ->
       (SimDeploymentComponent description'this initconfig'this
          (I'.fromIntegral instrPS'this)))
      ((truncate (lookupDefault initconfig'this Speed 0)) :: Int)

init'SimDeploymentComponent ::
                            Obj' SimDeploymentComponent -> I'.IO ()
{-# LINE 110 "..\habs-runtime\src\ABS\DC.abs" #-}
init'SimDeploymentComponent this@(Obj' this' _ thisDC) = I'.pure ()

instance DeploymentComponent' SimDeploymentComponent where
        load rtype periods this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        total rtype this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure (Fin 0))
        transfer target amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        decrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        incrementResources amount rtype this@(Obj' this' _ thisDC)
          = I'.pure ()
        getName this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' -> I'.pure (description'SimDeploymentComponent this''))
                    =<< I'.readIORef this')
        getCreationTime this@(Obj' this' _ thisDC) = do I'.lift (now)
        getStartupDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getShutdownDuration this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getPaymentInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getCostPerInterval this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        getNumberOfCores this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        acquire this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        release this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        shutdown this@(Obj' this' _ thisDC) = do I'.lift (I'.pure True)
        request__ nrInstr this@(Obj' this' _ thisDC)
          = do I'.lift (println (toString (I'.fromIntegral nrInstr)))
               (\ this'' ->
                  if
                    ((I'.fromIntegral nrInstr) >
                       (I'.fromIntegral (instrPS'SimDeploymentComponent this'')))
                    then
                    do I'.lift (duration 1 1)
                       suspend this
                       (\ this'' ->
                          this <..>
                            request__
                              ((I'.fromIntegral nrInstr) -
                                 (I'.fromIntegral (instrPS'SimDeploymentComponent this''))))
                         =<< I'.lift (I'.readIORef this')
                    else
                    do remaining :: IORef' Rat <- I'.lift
                                                    ((\ this'' ->
                                                        I'.newIORef
                                                          ((I'.fromIntegral nrInstr) /
                                                             (I'.fromIntegral
                                                                (instrPS'SimDeploymentComponent
                                                                   this''))))
                                                       =<< I'.readIORef this')
                       I'.lift
                         ((\ e1' -> duration e1' =<< I'.readIORef remaining) =<<
                            I'.readIORef remaining))
                 =<< I'.lift (I'.readIORef this')
main
  = main_is'
      (\ this@(Obj' _ _ thisDC) ->
         do o :: IORef' DeploymentComponent <- I'.lift
                                                 (((I'.newIORef . DeploymentComponent) =<<
                                                     new thisDC init'SimDeploymentComponent
                                                       (smart'SimDeploymentComponent "mplo"
                                                          (map [((Speed, 3))]))))
            f1 :: IORef' (Fut Unit) <- I'.lift
                                         (I'.newIORef =<<
                                            ((\ (DeploymentComponent obj') ->
                                                (obj' <!> request__ 10))
                                               =<< I'.readIORef o))
            f2 :: IORef' (Fut Unit) <- I'.lift
                                         (I'.newIORef =<<
                                            ((\ (DeploymentComponent obj') ->
                                                (obj' <!> request__ 19))
                                               =<< I'.readIORef o))
            awaitFutures' this
              [(I'.isEmptyMVar =<< I'.readIORef f1),
               (I'.isEmptyMVar =<< I'.readIORef f2)]
              ((I'.readMVar =<< I'.readIORef f1) *>
                 (I'.readMVar =<< I'.readIORef f2)))
