-- | I/O primitives of ABS to the terminal screen
module ABS.Runtime.Extension.IO 
  ( print, println, readln
  ) where


import Prelude hiding (print)

{-# INLINE print #-}
-- | Decided to be a statement and not a built-in function for keeping functions pure.
print :: String -> IO ()
print = putStr

{-# INLINE println #-}
-- | Decided to be a statement and not a built-in function for keeping functions pure.
println :: String -> IO ()
println = putStrLn

{-# INLINE readln #-}
-- | Decided to be a statement and not a built-in function for keeping functions pure.
readln :: IO String
readln = getLine
