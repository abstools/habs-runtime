module Main where

import Criterion.Main
import Criterion.Types

fib :: a -> a
fib = id

main :: IO ()
main = defaultMainWith benchConfig [
  bgroup "fib" [ bench "1"  $ whnf fib 1  -- nf, nfIO, whnfIO
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]

benchConfig :: Config
benchConfig = defaultConfig {
                reportFile = Just "dist/bench-results.html"
              -- , timeLimit = 5 -- the time window of successive runs for each benchmark, defaults to 5s per individual benchmark
           }
