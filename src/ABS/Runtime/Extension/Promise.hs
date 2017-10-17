{-# LANGUAGE DeriveDataTypeable #-}
-- | ABS extension to support write-once promises 
-- (used by the PA case study).
module ABS.Runtime.Extension.Promise 
  ( pro_new
  , pro_try
  , pro_give
  , PromiseRewriteException (..)
  ) where

import ABS.Runtime.Base (Fut)

import Control.Concurrent.MVar (newEmptyMVar, tryReadMVar, tryPutMVar)
import ABS.Runtime.Extension.Exception
import Data.Typeable -- exceptions must support Show and Typeable
import Control.Monad (unless)
import qualified Control.Exception (Exception (..))

{-# INLINE pro_new #-}
-- | Newly-allocated promise with empty contents 
pro_new :: IO (Fut a)
pro_new = newEmptyMVar

{-# INLINE pro_try #-}
-- | Try to extract the value inside the promise.
--
-- Does not block.
pro_try :: Fut b -> IO (Maybe b)
pro_try = tryReadMVar

{-# INLINE pro_give #-}
-- | Writes (once) the value to the given promise, essentially resolving it.
--
-- Will throw a 'PromiseRewriteException' when to write more than once.
pro_give :: Fut b -> b -> IO ()
pro_give fut = (>>= (`unless` throw PromiseRewriteException)) . tryPutMVar fut


-- | Trying to write to an already-resolved promise
data PromiseRewriteException = PromiseRewriteException deriving (Show, Typeable)
instance Control.Exception.Exception PromiseRewriteException where
  toException = absExceptionToException'
  fromException = absExceptionFromException'
