{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}
module ABS.Runtime.Extension.Exception 
    ( -- ^ the ABS commands in code generation
      throw, catch, Handler'(..), finally
    -- ^ same exceptions as Haskell's, so import them rather than redefining.
    -- This has the benefit of catching exceptions from the FFI
    , PatternMatchFail (..), RecSelError (..), ArithException (DivideByZero), AssertionFailed (..)
    -- ^ the ABS-specific builtin exceptions
    , NullException(..)
    , ABSException'(..), absExceptionToException', absExceptionFromException' -- the helper functions to construct instances of Exception (boilerplate)
    ) where

import ABS.Runtime.Base (ABS')
import Control.Monad.Trans.Cont (ContT (..), runContT)
import qualified Control.Monad.Catch
import Control.Exception (PatternMatchFail (..), RecSelError (..), ArithException (DivideByZero), AssertionFailed (..))
import Data.Typeable (cast)

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
        probe (Handler' h) xs = maybe xs (\ e' -> case h e' of
                                                    Nothing -> xs -- trick to not throw patternmatch fail here
                                                    Just action -> action)
                                (Control.Monad.Catch.fromException e)


-- the existential wrapper for synchronous  builtin&user-defined ABS exceptions
data ABSException' = forall e. Control.Monad.Catch.Exception e => ABSException' e
instance Show ABSException' where
    show (ABSException' e) = show e
instance Control.Monad.Catch.Exception ABSException'

-- conversion back and forth to the root existential exception (SomeException)
absExceptionToException' :: Control.Monad.Catch.Exception e => e -> Control.Monad.Catch.SomeException
absExceptionToException' = Control.Monad.Catch.toException . ABSException'
absExceptionFromException' :: Control.Monad.Catch.Exception e => Control.Monad.Catch.SomeException -> Maybe e
absExceptionFromException' x = do
    ABSException' a <- Control.Monad.Catch.fromException x
    cast a




-- BUILTIN ABS SYNC EXCEPTIONS

-- Inherited from haskell
-- divbyzero
-- patternmatchfail
-- assertionfailed
-- recselerror

-- ABS-exclusive exceptions
data NullException = NullException deriving Show

-- we don't need this, since we have a weak notion of "soft realtime": based on when it is put back to the enabled queue, not actual execution
--data DurationExpired = DurationExpired deriving Show
--instance Control.Monad.Catch.Exception DurationExpired where
--  toException = absExceptionToException'
--  fromException = absExceptionFromException'

-- boilerplate code
instance Control.Monad.Catch.Exception NullException where
  toException = absExceptionToException'
  fromException = absExceptionFromException'
