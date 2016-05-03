{-# LANGUAGE CPP #-}
module ABS.Runtime.Extension.Promise 
  ( pro_new
  , pro_try
  , pro_give
  ) where

import Control.Concurrent.MVar (newEmptyMVar, tryReadMVar, putMVar)

import ABS.Runtime.Base (Fut (..))

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

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
