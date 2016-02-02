{-# LANGUAGE DeriveDataTypeable #-}

module ABS.Runtime.Exception.Base where

-- for exceptions
import qualified Control.Monad.Catch
import Data.Typeable -- exceptions must be typeable

type Exception = Control.Monad.Catch.SomeException

-- * Builtin Exception-constructors for ABS
data BlockedAwaitException = BlockedAwaitException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception BlockedAwaitException

-- | Trying to write to an already-resolved promise
data PromiseRewriteException = PromiseRewriteException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception PromiseRewriteException
