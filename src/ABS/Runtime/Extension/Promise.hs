module ABS.Runtime.Extension.Promise 
  ( emptyFuture
  , try
  , resolve
  ) where

import Control.Concurrent.MVar (newEmptyMVar, tryReadMVar, putMVar)

import ABS.Runtime.Base (Fut (..))

{-# INLINE emptyFuture #-}
-- | empty future unlifted
emptyFuture :: IO (Fut a)
emptyFuture = Fut <$> newEmptyMVar

{-# INLINE try #-}
try :: Fut b -> IO (Maybe b)
try (Fut fut) = tryReadMVar fut

{-# INLINE resolve #-}
resolve :: Fut b -> b -> IO ()
resolve (Fut fut) = putMVar fut