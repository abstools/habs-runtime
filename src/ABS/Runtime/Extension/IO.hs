module ABS.Runtime.Extension.IO 
  ( print, println, readln
  ) where


import Prelude hiding (print)
import ABS.Runtime.Base (ABS')
import Control.Monad.Trans.Class (lift)

{-# INLINE print #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
print :: String -> IO ()
print = putStr

{-# INLINE println #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
println :: String -> IO ()
println = putStrLn

{-# INLINE readln #-}
-- | I decided to be a statement and not a built-in function for keeping functions pure.
--
-- Design choice: we leave it in ABS' monad so as not to allow it to run inside init block(and thus possibly make init to block on input)
readln :: ABS' String
readln = lift getLine
