{-# LANGUAGE DeriveDataTypeable #-}
module ABS.Runtime.Extension.Promise 
  ( pro_new
  , pro_try
  , pro_give
  ) where

import ABS.Runtime.Base (Fut)

import Control.Concurrent.MVar (newEmptyMVar, tryReadMVar, tryPutMVar)
import ABS.Runtime.Extension.Exception
import Data.Typeable -- exceptions must support Show and Typeable
import Control.Monad (unless)

{-# INLINE pro_new #-}
-- | empty future unlifted
pro_new :: IO (Fut a)
pro_new = newEmptyMVar

{-# INLINE pro_try #-}
pro_try :: Fut b -> IO (Maybe b)
pro_try = tryReadMVar

{-# INLINE pro_give #-}
pro_give :: Fut b -> b -> IO ()
pro_give fut = (>>= (`unless` throw PromiseRewriteException)) . tryPutMVar fut


-- | Trying to write to an already-resolved promise
data PromiseRewriteException = PromiseRewriteException deriving (Show, Typeable)
instance Exception PromiseRewriteException
