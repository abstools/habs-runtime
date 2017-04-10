{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
  PartialTypeSignatures, LambdaCase, OverloadedStrings,
  TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts#-}
module ABS.DC 
       (module ABS.Runtime.Base, -- only those imported from ABS.Runtime.Base
        SimDeploymentComponent(..), smart'SimDeploymentComponent,
        init'SimDeploymentComponent, CloudProvider'(..), CloudProvider(..),
        SimCloudProvider(..), smart'SimCloudProvider,
        init'SimCloudProvider)
       where

import ABS.Runtime.Base (DeploymentComponent(..), DeploymentComponent'(..), Resourcetype(..), InfRat(..), finvalue)

import ABS.StdLib
import ABS.Runtime
import Data.Function ((.))
import Control.Applicative ((<*>), (*>))
import Control.Monad ((=<<))
import qualified Control.Applicative as I' (pure)
import qualified Data.IORef as I'
       (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Control.Monad.Trans.Class as I' (lift)
import qualified Control.Monad as I' (when, sequence, join)
import qualified Prelude as I'
       (IO, Eq, Ord(..), Show(..), undefined, error, negate, fromIntegral,
        mapM_, id)
import qualified Unsafe.Coerce as I' (unsafeCoerce)
import qualified Control.Concurrent as I' (ThreadId)
import qualified Control.Concurrent.MVar as I'
       (isEmptyMVar, readMVar)
import Control.Exception (assert)
import qualified Control.Exception as I'
       (Exception(..), SomeException, throwTo, throw)
import qualified Data.Dynamic as I' (toDyn, fromDynamic)
import qualified Data.Map as I' (lookup)
import qualified Web.Scotty as I' (get, param, json, raise)
import qualified ABS.StdLib as I' (put)
import qualified Data.Generics.Genifunctors as I' (genFmap)

default (Int, Rat)

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
          instrPS'this))
      ((truncate (lookupDefault initconfig'this (Speed) 0)))

init'SimDeploymentComponent ::
                            Obj' SimDeploymentComponent -> I'.IO ()

init'SimDeploymentComponent this@(Obj' this' _ thisDC) = I'.pure ()

instance DeploymentComponent' SimDeploymentComponent where
        load rtype periods this@(Obj' this' _ thisDC)
          = do I'.lift (I'.pure 0)
        total rtype this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure
                       (case (lookup (initconfig'SimDeploymentComponent this'') rtype) of
                            Nothing -> (InfRat)
                            Just v -> (Fin v)))
                    =<< I'.readIORef this')
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
        acquire this@(Obj' this' _ thisDC) = do I'.lift (I'.pure (True))
        release this@(Obj' this' _ thisDC) = do I'.lift (I'.pure (True))
        shutdown this@(Obj' this' _ thisDC) = do I'.lift (I'.pure (True))
        request' nrInstr this@(Obj' this' _ thisDC)
          = do input :: IORef' Int <- I'.lift (I'.newIORef nrInstr)
               while
                 ((\ this'' ->
                     ((>) <$!> I'.readIORef input <*>
                        I'.pure (instrPS'SimDeploymentComponent this'')))
                    =<< I'.readIORef this')
                 (do I'.lift (duration 1 1)
                     suspend this
                     I'.lift
                       (I'.writeIORef input =<<
                          (\ this'' ->
                             ((-) <$!> I'.readIORef input <*>
                                I'.pure (instrPS'SimDeploymentComponent this'')))
                            =<< I'.readIORef this'))
               remaining :: IORef' Rat <- I'.lift
                                            ((\ this'' ->
                                                I'.newIORef
                                                  ((up' nrInstr :: Rat) /
                                                     (up' (instrPS'SimDeploymentComponent this'') ::
                                                        Rat)))
                                               =<< I'.readIORef this')
               I'.lift
                 ((\ e1' -> duration e1' =<< I'.readIORef remaining) =<<
                    I'.readIORef remaining)
               I'.pure ()


class CloudProvider' a where
        
        prelaunchInstance ::
                          Map Resourcetype Rat -> Obj' a -> ABS' DeploymentComponent
        
        
        launchInstance ::
                       Map Resourcetype Rat -> Obj' a -> ABS' DeploymentComponent
        
        
        acquireInstance_ :: DeploymentComponent -> Obj' a -> ABS' Bool
        
        
        shutdownInstance :: DeploymentComponent -> Obj' a -> ABS' Bool
        
        
        setInstanceDescriptions ::
                                Map String (Map Resourcetype Rat) -> Obj' a -> ABS' Unit
        
        
        addInstanceDescription ::
                               Pair String (Map Resourcetype Rat) -> Obj' a -> ABS' Unit
        
        
        removeInstanceDescription :: String -> Obj' a -> ABS' Unit
        
        
        getInstanceDescriptions ::
                                Obj' a -> ABS' (Map String (Map Resourcetype Rat))
        
        
        prelaunchInstanceNamed ::
                               String -> Obj' a -> ABS' DeploymentComponent
        
        
        launchInstanceNamed :: String -> Obj' a -> ABS' DeploymentComponent

data CloudProvider = forall a . CloudProvider' a =>
                       CloudProvider (Obj' a)

instance I'.Show CloudProvider where
        show _ = "CloudProvider"

instance I'.Eq CloudProvider where
        CloudProvider (Obj' ref1' _ _) == CloudProvider (Obj' ref2' _ _)
          = ref1' == I'.unsafeCoerce ref2'

instance CloudProvider' Null' where
        prelaunchInstance = I'.undefined
        launchInstance = I'.undefined
        acquireInstance_ = I'.undefined
        shutdownInstance = I'.undefined
        setInstanceDescriptions = I'.undefined
        addInstanceDescription = I'.undefined
        removeInstanceDescription = I'.undefined
        getInstanceDescriptions = I'.undefined
        prelaunchInstanceNamed = I'.undefined
        launchInstanceNamed = I'.undefined

instance CloudProvider' a => Sub' (Obj' a) CloudProvider where
        up' = CloudProvider


data SimCloudProvider = SimCloudProvider{acquiredInstances'SimCloudProvider
                                         :: List DeploymentComponent,
                                         instanceDescriptions'SimCloudProvider ::
                                         Map String (Map Resourcetype Rat),
                                         keeprunning'SimCloudProvider :: Bool,
                                         killedInstances'SimCloudProvider ::
                                         List DeploymentComponent,
                                         launchedInstances'SimCloudProvider ::
                                         List DeploymentComponent,
                                         name'SimCloudProvider :: String,
                                         nextInstanceId'SimCloudProvider :: Int}

smart'SimCloudProvider :: String -> SimCloudProvider
smart'SimCloudProvider name'this
  = (\ instanceDescriptions'this ->
       (\ launchedInstances'this ->
          (\ acquiredInstances'this ->
             (\ killedInstances'this ->
                (\ nextInstanceId'this ->
                   (\ keeprunning'this ->
                      (SimCloudProvider acquiredInstances'this instanceDescriptions'this
                         keeprunning'this
                         killedInstances'this
                         launchedInstances'this
                         name'this
                         nextInstanceId'this))
                     ((True)))
                  (0))
               ((list [])))
            ((list [])))
         ((list [])))
      ((map []))

init'SimCloudProvider :: Obj' SimCloudProvider -> I'.IO ()

init'SimCloudProvider this@(Obj' this' _ thisDC) = I'.pure ()

instance CloudProvider' SimCloudProvider where
        prelaunchInstance d this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<<
                                                         (\ this'' ->
                                                            (this <..>
                                                               createInstance''SimCloudProvider
                                                                 (name'SimCloudProvider this'')
                                                                 d))
                                                           =<< I'.lift (I'.readIORef this')
               startup_duration :: IORef' Rat <- I'.lift
                                                   (I'.newIORef I'.undefined)
               (\ (DeploymentComponent obj') ->
                  awaitSugar' this (I'.writeIORef startup_duration) obj'
                    (getStartupDuration))
                 =<< I'.lift (I'.readIORef result)
               (\ e1' ->
                  awaitDuration' this e1' =<<
                    I'.lift
                      ((minimum <$!> (I'.sequence [I'.readIORef startup_duration]))))
                 =<<
                 I'.lift
                   ((maximum <$!> (I'.sequence [I'.readIORef startup_duration])))
               I'.lift (I'.readIORef result)
        launchInstance d this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<< (this <..> prelaunchInstance d)
               I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        (\ v' -> this''{acquiredInstances'SimCloudProvider = v'}) <$!>
                          ((:) <$!> I'.readIORef result <*>
                             I'.pure (acquiredInstances'SimCloudProvider this'')))
                       =<< I'.readIORef this'))
               I'.lift (I'.readIORef result)
        acquireInstance_ instance_ this@(Obj' this' _ thisDC)
          = do result :: IORef' Bool <- I'.lift (I'.newIORef (True))
               I'.lift (I'.pure (False))
        shutdownInstance instance_ this@(Obj' this' _ thisDC)
          = do _ <- I'.lift
                      ((\ (DeploymentComponent obj') -> (obj' <!!> shutdown)) instance_)
               I'.lift (I'.pure (True))
        setInstanceDescriptions instanceDescriptions
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 instanceDescriptions})
                       <$!> I'.readIORef this'))
               I'.pure ()
        addInstanceDescription instanceDescription
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 (insert (instanceDescriptions'SimCloudProvider this'')
                                    instanceDescription)})
                       <$!> I'.readIORef this'))
               I'.pure ()
        removeInstanceDescription instanceDescriptionName
          this@(Obj' this' _ thisDC)
          = do I'.lift
                 (I'.writeIORef this' =<<
                    ((\ this'' ->
                        this''{instanceDescriptions'SimCloudProvider =
                                 (removeKey (instanceDescriptions'SimCloudProvider this'')
                                    instanceDescriptionName)})
                       <$!> I'.readIORef this'))
               I'.pure ()
        getInstanceDescriptions this@(Obj' this' _ thisDC)
          = do I'.lift
                 ((\ this'' ->
                     I'.pure (instanceDescriptions'SimCloudProvider this''))
                    =<< I'.readIORef this')
        prelaunchInstanceNamed instancename this@(Obj' this' _ thisDC)
          = do mconfig :: IORef' (Maybe (Map Resourcetype Rat)) <- I'.lift
                                                                     ((\ this'' ->
                                                                         I'.newIORef
                                                                           (lookup
                                                                              (instanceDescriptions'SimCloudProvider
                                                                                 this'')
                                                                              instancename))
                                                                        =<< I'.readIORef this')
               dc :: IORef' DeploymentComponent <- I'.lift
                                                     (I'.newIORef (up' null))
               when' <- I'.lift ((I'.pure isJust <*> I'.readIORef mconfig))
               I'.when when'
                 (do config :: IORef' (Map Resourcetype Rat) <- I'.lift
                                                                  (I'.newIORef =<<
                                                                     (I'.pure fromJust <*>
                                                                        I'.readIORef mconfig))
                     (I'.lift . I'.writeIORef dc) =<<
                       ((this <..>) =<<
                          I'.lift
                            ((\ this'' ->
                                I'.pure createInstance''SimCloudProvider <*>
                                  ((+) <$!>
                                     ((+) <$!> I'.pure (name'SimCloudProvider this'') <*>
                                        I'.pure "-")
                                     <*> I'.pure instancename)
                                  <*> I'.readIORef config)
                               =<< I'.readIORef this')))
               startup_duration :: IORef' Rat <- I'.lift
                                                   (I'.newIORef I'.undefined)
               (\ (DeploymentComponent obj') ->
                  awaitSugar' this (I'.writeIORef startup_duration) obj'
                    (getStartupDuration))
                 =<< I'.lift (I'.readIORef dc)
               (\ e1' ->
                  awaitDuration' this e1' =<<
                    I'.lift
                      ((minimum <$!> (I'.sequence [I'.readIORef startup_duration]))))
                 =<<
                 I'.lift
                   ((maximum <$!> (I'.sequence [I'.readIORef startup_duration])))
               I'.lift (I'.readIORef dc)
        launchInstanceNamed instancename this@(Obj' this' _ thisDC)
          = do result :: IORef' DeploymentComponent <- (I'.lift .
                                                          I'.newIORef)
                                                         =<<
                                                         (this <..>
                                                            prelaunchInstanceNamed instancename)
               when' <- I'.lift
                          ((not <$!>
                              ((==) (DeploymentComponent null) <$!> I'.readIORef result)))
               I'.when when'
                 (do I'.lift
                       (I'.writeIORef this' =<<
                          ((\ this'' ->
                              (\ v' -> this''{acquiredInstances'SimCloudProvider = v'}) <$!>
                                ((:) <$!> I'.readIORef result <*>
                                   I'.pure (acquiredInstances'SimCloudProvider this'')))
                             =<< I'.readIORef this')))
               I'.lift (I'.readIORef result)

createInstance''SimCloudProvider ::
                                 String ->
                                   Map Resourcetype Rat ->
                                     Obj' SimCloudProvider -> ABS' DeploymentComponent
createInstance''SimCloudProvider instancename d
  this@(Obj' this' _ thisDC)
  = do result :: IORef' DeploymentComponent <- I'.lift
                                                 ((\ this'' ->
                                                     ((I'.newIORef . DeploymentComponent) =<<
                                                        new thisDC init'SimDeploymentComponent
                                                          (smart'SimDeploymentComponent
                                                             ((instancename + "-") +
                                                                (toString
                                                                   (nextInstanceId'SimCloudProvider
                                                                      this'')))
                                                             d)))
                                                    =<< I'.readIORef this')
       I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                this''{nextInstanceId'SimCloudProvider =
                         ((nextInstanceId'SimCloudProvider this'') + 1)})
               <$!> I'.readIORef this'))
       stupidTypeSystem :: IORef' DeploymentComponent <- I'.lift
                                                           (I'.newIORef =<< I'.readIORef result)
       I'.lift
         (I'.writeIORef this' =<<
            ((\ this'' ->
                (\ v' -> this''{launchedInstances'SimCloudProvider = v'}) <$!>
                  ((:) <$!> I'.readIORef stupidTypeSystem <*>
                     I'.pure (launchedInstances'SimCloudProvider this'')))
               =<< I'.readIORef this'))
       I'.lift (I'.readIORef result)