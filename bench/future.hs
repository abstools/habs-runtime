module Main where

import Criterion.Main
import Criterion.Types
import Control.Concurrent (threadDelay, newEmptyMVar, forkIO, putMVar, readMVar)
import Control.Monad (replicateM, replicateM_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

{- Setup
n async method calls each with m listeners for their values

each listener lives in its a seperate green thread

-}

method :: IO Int
method = do
  threadDelay 1000
  return 3

main1 :: Int -> Int -> IO ()
main1 n m = do
  fs <- replicateM n (do
                 f <- newEmptyMVar
                 _ <- forkIO (do 
                          res <- method
                          putMVar f res)
                 replicateM_ m (forkIO $ readMVar f >> return ())
                 return f)
  mapM_ (\ f -> readMVar f >> return ()) fs

main2 :: Int -> Int -> IO ()
main2 n m = do
  fs <- replicateM n (do
                 f <- newEmptyTMVarIO
                 _ <- forkIO (do 
                          res <- method
                          atomically $ putTMVar f res)
                 replicateM_ m (forkIO $ (atomically (readTMVar f)) >> return ())
                 return f)
  mapM_ (\ f -> (atomically (readTMVar f)) >> return ()) fs

               
main :: IO ()
main = defaultMainWith benchConfig [
         bgroup "1-1" [ bench "mvar"  $ whnfIO $ main1 1 1
                      , bench "tmvar"  $ whnfIO $ main2 1 1
                      ]
       , bgroup "1-10" [ bench "mvar"  $ whnfIO $ main1 1 10
                       , bench "tmvar"  $ whnfIO $ main2 1 10
                      ]
       , bgroup "1-100" [  bench "mvar"  $ whnfIO $ main1 1 100
                        , bench "tmvar"  $ whnfIO $ main2 1 100
                        ]
       , bgroup "2-1" [  bench "mvar"  $ whnfIO $ main1 2 1
                      , bench "tmvar"  $ whnfIO $ main2 2 1
                      ]
       , bgroup "2-10" [  bench "mvar"  $ whnfIO $ main1 2 10
                       , bench "tmvar"  $ whnfIO $ main2 2 10
                      ]
       , bgroup "2-100" [  bench "mvar"  $ whnfIO $ main1 2 100
                        , bench "tmvar"  $ whnfIO $ main2 2 100
                        ]
       , bgroup "4-1" [  bench "mvar"  $ whnfIO $ main1 4 1
                      , bench "tmvar"  $ whnfIO $ main2 4 1
                      ]
       , bgroup "4-10" [  bench "mvar"  $ whnfIO $ main1 4 10
                       , bench "tmvar"  $ whnfIO $ main2 4 10
                      ]
       , bgroup "4-100" [  bench "mvar"  $ whnfIO $ main1 4 100
                        , bench "tmvar"  $ whnfIO $ main2 4 100
                      ]
         ]

benchConfig :: Config
benchConfig = defaultConfig {
                reportFile = Just "dist/bench-future-results.html"
              , csvFile = Just "dist/bench-future-results.csv"
              -- , timeLimit = 5 -- the time window of successive runs for each benchmark, defaults to 5s per individual benchmark
           }
