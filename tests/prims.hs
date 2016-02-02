module Main where

import ABS.Runtime.Base
import ABS.Runtime.Prim

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html
import System.IO.Silently (capture)

import System.IO
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

main = defaultMainWithIngredients (htmlRunner:defaultIngredients) $
       localOption (mkTimeout 1000000) $ -- timeouts any test at 1s
       testGroup "coop-scheduling"
                     [ testCase "case_FIFO" case_FIFO
                     , testCase "test2" test2
                     ]

data C = C                      -- an obj empty contents

case_FIFO :: IO ()
case_FIFO = do
  hSetBuffering stdout LineBuffering

  let method1 this = do
          liftIO $ threadDelay 10     
          println "m1"

  let method2 this = do
          liftIO $ threadDelay 10
          println "m2"

  let main = main_is' (\ this -> do
                o1 <- newlocal C (const $ return ()) this
                o2 <- newlocal C (const $ return ()) this
                replicateM_ 1000 (do
                                 o1 <!!> method1
                                 o2 <!!> method2
                               )
                f1 <- o1 <!> (\ _this -> skip) -- dummy method call so main does not exit too-early
                awaitFuture f1 this
                      )

  (outStr, ()) <- capture main
  let outLines = lines outStr
  -- assertEqual "Async method calls not in COG-FIFO order" (take 2000 $ lines out_str) (take 2000 $ cycle ["m1","m2"])
  assertEqual "Wrong size of output" 2000 (length $ outLines)
  assertEqual "Wrong nr. of m1 method calls" 1000 (countElem "m1" $ outLines)
  assertEqual "Wrong nr. of m2 method calls" 1000 (countElem "m2" $ outLines)

test2 :: IO ()
test2 = do
  return ()


countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

