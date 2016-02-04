module ABS.Runtime.Exception.Prim 
    (
    -- * The failure model
    throw', catches', finally', Exception, assert'
    ) 
    where

-- | The ABS assertions. 
assert :: ABS Prelude.Bool -> ABS ()
assert act = act Prelude.>>= \ pred -> when (Prelude.not pred) 
             (throw' $ return $ Control.Exception.AssertionFailed "Assertion Failed")

{-# INLINE throw' #-}
-- | aliases for easier exporting
throw :: Control.Monad.Catch.Exception e => ABS e -> ABS a
throw e = e >>= Control.Monad.Catch.throwM

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catch :: ABS a -> [Control.Monad.Catch.Handler ABS (Maybe a)] -> ABS a
catch a hs = a `Control.Monad.Catch.catch` handler
  where
    handler e = foldr probe (Control.Monad.Catch.throwM e) hs
      where
        probe (Control.Monad.Catch.Handler h) xs = maybe xs (\ e -> h e >>= (\ x -> case x of
                                                                              Nothing -> xs -- trick to allow actual term pattern-matching
                                                                              Just res -> return res))
                                                                              (Control.Exception.fromException e)


{-# INLINE finally #-}
finally :: ABS a -> ABS b -> ABS a
finally = Control.Monad.Catch.finally
