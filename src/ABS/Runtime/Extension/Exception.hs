{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}
module ABS.Runtime.Extension.Exception 
    ( Control.Monad.Catch.Exception -- the typeclass
    , throw, catch, Handler'(..), finally
    ) where

import ABS.Runtime.Base (ABS')
import Control.Monad.Trans.Cont (ContT (..), runContT)
import qualified Control.Monad.Catch


{-# INLINE throw #-}
throw :: (Control.Monad.Catch.MonadThrow m , Control.Monad.Catch.Exception e) => e -> m a 
throw = Control.Monad.Catch.throwM

instance Control.Monad.Catch.MonadCatch ABS' where
  m `catch` f = ContT $ \c -> runContT m c `Control.Monad.Catch.catch` \e -> runContT (f e) c -- taken from package MonadCatchIO-transformers

-- we do not have async. exceptions in ABS world, so we do not have to implement mask (and its related uninterruptibleMask)
-- instance Control.Monad.Catch.MonadMask ABS where

-- | Our ABS finally is different than default Haskell's, since we do not have async. exceptions, thus we do not need to mask
finally :: Control.Monad.Catch.MonadCatch m => m () -> m () -> m ()
stm `finally` stmAfter = do
  stm `Control.Monad.Catch.onException` stmAfter
  stmAfter
  
-- | different handler because we want maybe (for ABS exception pattern matching)
data Handler' m a = forall e . Control.Monad.Catch.Exception e => Handler' (e -> Maybe (m a))

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catch :: Control.Monad.Catch.MonadCatch m => m a -> [Handler' m a] -> m a
catch a hs = a `Control.Monad.Catch.catch` handler
  where
    handler e = foldr probe (Control.Monad.Catch.throwM e) hs
      where
        probe (Handler' h) xs = maybe xs (\ e -> case h e of
                                                  Nothing -> xs -- trick to not throw patternmatch fail here
                                                  Just action -> action)
                              (Control.Monad.Catch.fromException e)
