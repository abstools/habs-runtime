module ABS.Runtime.Extension.Promise 
  ( pro_new
  , pro_try
  , pro_give
  ) where

import ABS.Runtime.Base (Fut (..))

import Control.Concurrent.MVar (newEmptyMVar, tryReadMVar, putMVar)


{-# INLINE pro_new #-}
-- | empty future unlifted
pro_new :: IO (Fut a)
pro_new = Fut <$> newEmptyMVar

{-# INLINE pro_try #-}
pro_try :: Fut b -> IO (Maybe b)
pro_try (Fut fut) = tryReadMVar fut

{-# INLINE pro_give #-}
pro_give :: Fut b -> b -> IO ()
pro_give (Fut fut) = putMVar fut
