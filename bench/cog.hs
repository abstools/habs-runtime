module Main where

import ABS.Runtime.Base
import ABS.Runtime.Prim

import Criterion.Main
import Criterion.Types

import Data.IORef
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT, callCC)
import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import System.Environment (withArgs)

{- Setup
 n COGs, m procs, j loops of procs

1 COG, 1 proc each, 100 loops each
1 COG, 100 procs each, 100 loops each
4 COGs, 1 proc each, 100 loops each
4 COGs, 100 procs each, 100 loops each
100 COGs, 1 proc each, 100 loops each
100 COGs, 100 procs each, 100 loops each

-}

method7 :: Int -> IORef Int -> Obj' this -> ABS' ()
method7 j attr this = do
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    if (v > j)
     then return ()
     else do
       suspend this
       this <..> method7 j attr
    
data C = C

main7 n m j = withArgs [] $ main_is' (\ this -> do
                          fs <-replicateM n (liftIO $ do
                                                 obj <- new (const $ return ()) C
                                                 replicateM m (do
                                                                attr <- newIORef 0
                                                                obj <!> method7 j attr)
                                               )
                          mapM_ (\ f -> awaitFuture' this f) (concat fs)
                       )


method1 :: Int -> IORef Int -> MVar () -> ABS' ()
method1 j attr t = do
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    if (v > j)
     then return ()
     else do
       liftIO (putMVar t () >> takeMVar t) -- = suspend
       method1 j attr t
    
main1 n m j =  do
  fs <- replicateM n (do
                      cog_token <- newMVar ()
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     _ <- forkIO $ do
                                           takeMVar cog_token
                                           evalContT $ do
                                                    res <- method1 j attr cog_token
                                                    liftIO $ putMVar destiny res
                                                    liftIO $ putMVar cog_token ()
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


method2 :: Int -> IORef Int -> MVar () -> IO ()
method2 j attr t = do
    v <- readIORef attr
    writeIORef attr $! v + 1
    v <- readIORef attr
    writeIORef attr $! v + 1
    if (v > j)
     then return ()
     else do
       putMVar t () >> takeMVar t -- = suspend
       method2 j attr t
    
main2 n m j =  do
  fs <- replicateM n (do
                      cog_token <- newMVar ()
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     _ <- forkIO $ do
                                       takeMVar cog_token
                                       res <- method2 j attr cog_token
                                       putMVar destiny res
                                       putMVar cog_token ()
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


method3 :: Int -> IORef Int -> Chan (() -> ABS' ()) -> ABS' ()
method3 j attr t = do
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    if (v > j)
     then return () -- v should be 101
     else do
       callCC (\ k -> do
                   liftIO $ writeChan t k
                   k' <- liftIO $ readChan t
                   k' ()
              )
       method3 j attr t
    
main3 n m j =  do
  fs <- replicateM n (do
                      cog_chan <- newChan
                      forkIO (do
                               k <- readChan cog_chan
                               evalContT (k ()))
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     writeChan cog_chan (\ () -> do
                                                          res <- method3 j attr cog_chan
                                                          liftIO $ putMVar destiny res
                                                          k' <- liftIO $ readChan cog_chan
                                                          k' ()
                                                        )
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


method4 :: Int -> IORef Int -> (U.InChan (() -> ABS' ()), U.OutChan (() -> ABS' ())) -> ABS' ()
method4 j attr c@(i,o) = do
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    if (v > j)
     then return () -- v should be 101
     else do
       callCC (\ k -> do
                   liftIO $ U.writeChan i k
                   k' <- liftIO $ U.readChan o
                   k' ()
              )
       method4 j attr c
    
main4 n m j =  do
  fs <- replicateM n (do
                      c@(i,o) <- U.newChan
                      forkIO (do
                               k <- U.readChan o
                               evalContT (k ()))
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     U.writeChan i (\ () -> do
                                                          res <- method4 j attr c
                                                          liftIO $ putMVar destiny res
                                                          k' <- liftIO $ U.readChan o
                                                          k' ()
                                                        )
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


method5 :: Int -> IORef Int -> TQueue (() -> ABS' ()) -> ABS' ()
method5 j attr t = do
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    v <- liftIO $ readIORef attr
    liftIO $ writeIORef attr $! v + 1
    if (v > j)
     then return () -- v should be 101
     else do
       callCC (\ k -> do
                   liftIO $ atomically $ writeTQueue t k
                   k' <- liftIO $ atomically $ readTQueue t
                   k' ()
              )
       method5 j attr t
    
main5 n m j =  do
  fs <- replicateM n (do
                      cog_chan <- newTQueueIO
                      forkIO (do
                               k <- atomically $ readTQueue cog_chan
                               evalContT (k ()))
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     atomically $ writeTQueue cog_chan (\ () -> do
                                                          res <- method5 j attr cog_chan
                                                          liftIO $ putMVar destiny res
                                                          k' <- liftIO $ atomically $ readTQueue cog_chan
                                                          k' ()
                                                        )
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


method6 :: Int -> IORef Int -> TQueue (() -> IO ()) -> (() -> IO ()) -> IO ()
method6 j attr t k = do
    v <- readIORef attr
    writeIORef attr $! v + 1
    v <- readIORef attr
    writeIORef attr $! v + 1
    if (v > j)
     then k () -- v should be 101
     else do
       atomically $ writeTQueue t (\ () -> method6 j attr t k)
       k' <- atomically $ readTQueue t
       k' ()


main6 n m j =  do
  fs <- replicateM n (do
                      cog_chan <- newTQueueIO
                      forkIO (do
                               k <- atomically $ readTQueue cog_chan
                               k ())
                      replicateM m (do
                                     attr <- newIORef 0
                                     destiny <- newEmptyMVar
                                     atomically $ writeTQueue cog_chan (\ () -> do
                                                          method6 j attr cog_chan (\ res ->do
                                                                                     putMVar destiny res
                                                                                     k' <- atomically $ readTQueue cog_chan
                                                                                     k' ())
                                                        )
                                     return destiny
                                   )
                    )
  mapM_ (\ f -> takeMVar f) (concat fs)


main :: IO ()
main = defaultMainWith benchConfig [
         bgroup "1-1" [ bench "proc w cont"  $ whnfIO $ main1 1 1 100
                      , bench "proc wo cont" $ whnfIO $ main2 1 1 100
                      , bench "cont-chan" $ whnfIO $ main3 1 1 100
                      , bench "cont-unagi" $ whnfIO $ main4 1 1 100
                      , bench "cont-tqueue" $ whnfIO $ main5 1 1 100
                      , bench "manual-cont-tqueue" $ whnfIO $ main6 1 1 100
                      , bench "our-runtime-tqueue" $ whnfIO $ main7 1 1 100
                      ]
       , bgroup "1-100" [ bench "proc w cont" $ whnfIO $ main1 1 100 100
                        , bench "proc wo cont" $ whnfIO $ main2 1 100 100
                        , bench "cont-chan" $ whnfIO $ main3 1 100 100
                        , bench "cont-unagi" $ whnfIO $ main4 1 100 100
                        , bench "cont-tqueue" $ whnfIO $ main5 1 100 100
                        , bench "manual-cont-tqueue" $ whnfIO $ main6 1 100 100
                        , bench "our-runtime-tqueue" $ whnfIO $ main7 1 100 100
                        ]
       , bgroup "2-1" [ bench "proc w cont" $ whnfIO $ main1 2 1 100
                      , bench "proc wo cont" $ whnfIO $ main2 2 1 100
                      , bench "cont-chan" $ whnfIO $ main3 2 1 100
                      , bench "cont-unagi" $ whnfIO $ main4 2 1 100
                      , bench "cont-tqueue" $ whnfIO $ main5 2 1 100
                      , bench "manual-cont-tqueue" $ whnfIO $ main6 2 1 100
                      , bench "our-runtime-tqueue" $ whnfIO $ main7 2 1 100
                      ]
       , bgroup "2-100" [ bench "proc w cont" $ whnfIO $ main1 2 100 100
                        , bench "proc wo cont" $ whnfIO $ main2 2 100 100
                        , bench "cont-chan" $ whnfIO $ main3 2 100 100
                        , bench "cont-unagi" $ whnfIO $ main4 2 100 100
                        , bench "cont-tqueue" $ whnfIO $ main5 2 100 100
                        , bench "manual-cont-tqueue" $ whnfIO $ main6 2 100 100
                        , bench "our-runtime-tqueue" $ whnfIO $ main7 2 100 100
                        ]
       , bgroup "4-1" [ bench "proc w cont" $ whnfIO $ main1 4 1 100
                        , bench "proc wo cont" $ whnfIO $ main2 4 1 100
                        , bench "cont-chan" $ whnfIO $ main3 4 1 100
                        , bench "cont-unagi" $ whnfIO $ main4 4 1 100
                        , bench "cont-tqueue" $ whnfIO $ main5 4 1 100
                        , bench "manual-cont-tqueue" $ whnfIO $ main6 4 1 100
                        , bench "our-runtime-tqueue" $ whnfIO $ main7 4 1 100
                        ]
       , bgroup "4-100" [ bench "proc w cont" $ whnfIO $ main1 4 100 100
                        , bench "proc wo cont" $ whnfIO $ main2 4 100 100
                        , bench "cont-chan" $ whnfIO $ main3 4 100 100
                        , bench "cont-unagi" $ whnfIO $ main4 4 100 100
                        , bench "cont-tqueue" $ whnfIO $ main5 4 100 100
                        , bench "manual-cont-tqueue" $ whnfIO $ main6 4 100 100
                        , bench "our-runtime-tqueue" $ whnfIO $ main7 4 100 100
                        ]
       ]

benchConfig :: Config
benchConfig = defaultConfig {
                reportFile = Just "dist/bench-cog-results.html"
              -- , timeLimit = 5 -- the time window of successive runs for each benchmark, defaults to 5s per individual benchmark
           }
