module Main where

import ABS.Runtime

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html
import System.IO.Silently (hCapture)
import System.Environment (getArgs, withArgs)

import System.IO
import Control.Monad (replicateM_, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, modifyIORef')

main :: IO ()
main = do
  args <- getArgs -- append to stdin args
  withArgs ("-j1":args) $ -- don't run tests in parallel because it messes output
       hSetBuffering stderr LineBuffering >>
       defaultMainWithIngredients (htmlRunner:defaultIngredients)(
         localOption (mkTimeout 1000000) $ -- timeouts any test at 1s
         testGroup "coop-scheduling"
                     [ testCase "case_fifo" case_fifo
                     , testCase "case_future_forwarding" case_future_forwarding
                     , testCase "case_await_boolean" case_await_boolean
                     ])

data C = C { x :: Int} -- an obj with 1 attribute
-- object's smart constructor
c' = C { x = 0}

-- we have to use stderr stream because tasty uses stdout and interferes
println' :: String -> ABS' ()
println' = liftIO . hPutStrLn stderr

case_fifo :: IO ()
case_fifo = do
  let method1 this = do
          liftIO $ threadDelay 10     
          suspend this
          println' "m1"

  let method2 this = do
          liftIO $ threadDelay 10
          suspend this
          println' "m2"

  let main = withArgs [] $ main_is' id (\  this -> do
                o1 <- lift $ newlocal' this (const $ return ()) c'
                o2 <- lift $ newlocal' this (const $ return ()) c'
                fs <- replicateM 100 (liftIO $ do
                                       f1 <- async' this o1 method1
                                       f2 <- async' this o2 method2
                                       return [f1,f2]
                                     )
                mapM_ (\ f -> awaitFuture' this f) (concat fs) -- so that main does not exit too early
                      )
                                
  (outStr, ()) <- hCapture [stderr] main
  let outLines = lines outStr
  assertEqual "Wrong size of output" 200 (length $ outLines)
  assertEqual "Async method calls not in COG-FIFO order" (take 200 $ cycle ["m1","m2"]) outLines
  assertEqual "Wrong nr. of m1 method calls" 100 (countElem "m1" $ outLines)
  assertEqual "Wrong nr. of m2 method calls" 100 (countElem "m2" $ outLines)

case_future_forwarding :: IO ()
case_future_forwarding = do
  let method1 this = do
          liftIO $ threadDelay 10
          suspend this
          return (3 :: Int)

  let method2 f this = do
          awaitFuture' this f
          res <- lift $ get f
          let res' = res + 1
          println' (show res')
          return res'

  let main = withArgs [] $ main_is' id (\  this -> do
                o1 <- lift $ new (const $ return ()) c'
                o2 <- lift $ new (const $ return ()) c'
                replicateM_ 100 (do
                                       f1 <- liftIO $ async' this o1 method1
                                       f2 <- liftIO $ async' this o2 (method2 f1)
                                       awaitFuture' this f2
                                       res <- lift $ get f1
                                       liftIO $ threadDelay 10
                                       println' (show res)
                                     )
                      )
                                
  (outStr, ()) <- hCapture [stderr] main
  let outLines = lines outStr
  assertEqual "Interleaving of future forwarding err'ed" (take 200 $ cycle ["4","3"]) outLines

case_await_boolean :: IO ()
case_await_boolean = do
  let inc this@(Obj' contents _ _) = do
          liftIO $ threadDelay 10
          awaitBool' this (\ C { x = x } -> x == 0)             
          liftIO $ modifyIORef' contents (\ C { x = x } -> C { x = x + 1})             
          println' "inc"
          suspend this
          return ()

  let dec this@(Obj' contents _ _) = do
          liftIO $ threadDelay 10
          awaitBool' this (\ C { x = x } -> x == 1)             
          liftIO $ modifyIORef' contents (\ C { x = x } -> C { x = x - 1})             
          println' "dec"
          suspend this
          return ()

  let check this@(Obj' contents _ _) = do
          C { x = x } <- liftIO $ readIORef contents             
          return x

  let main = withArgs [] $ main_is' id (\  this -> do
                o1 <- lift $ new (const $ return ()) c'
                fs <- replicateM 100 (liftIO $ do
                                 f1 <- async' this o1 dec
                                 f2 <- async' this o1 inc
                                 return [f1,f2]
                                    )
                mapM_ (\ f -> awaitFuture' this f) (concat fs)
                liftIO $ threadDelay 1000
                f <- liftIO $ async' this o1 check
                res <- lift $ get f
                liftIO $ assertEqual "wrong last value" 0 res
                      )

  let main_local = withArgs [] $ main_is' id (\  this -> do
                o1 <- lift $ newlocal' this (const $ return ()) c'
                fs <- replicateM 100 (liftIO $ do
                                 f1 <- async' this o1 dec
                                 f2 <- async' this o1 inc
                                 return [f1,f2]
                                    )
                mapM_ (\ f -> awaitFuture' this f) (concat fs)
                liftIO $ threadDelay 1000
                f <- liftIO $ async' this o1 check
                awaitFuture' this f
                res <- lift $ get f
                liftIO $ assertEqual "wrong last value" 0 res
                            )

  (outStr, ()) <- hCapture [stderr] main
  let outLines = lines outStr

  (outStr_local, ()) <- hCapture [stderr] main_local
  let outLines_local = lines outStr_local

  assertEqual "Interleaving of boolean-awaits err'ed" (take 200 $ cycle ["inc","dec"]) outLines

  assertEqual "Local interleaving of boolean-awaits err'ed" (take 200 $ cycle ["inc","dec"]) outLines_local

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

