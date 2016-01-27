module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

main = defaultMainWithIngredients (htmlRunner:defaultIngredients) $
       localOption (mkTimeout 1000000) $ -- timeouts any test at 1s
       testGroup "prims"
                     [ testCase "test1" test1
                     , testCase "test2" test2
                     ]

test1 :: IO ()
test1 = do
  assertBool "mplo" True
  assertEqual "mpli" 3 3
  if (4 > 3)
    then return ()
    else assertFailure "this is for unconditional failure"
  return ()

test2 :: IO ()
test2 = do
  return ()
