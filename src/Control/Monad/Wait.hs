{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Wait(
    MonadWait(..)
) where


import           Control.Concurrent     (threadDelay)
import           Control.Exception.Safe
import           Control.Monad.Trans

class (Monad m, MonadThrow m) => MonadWait m where
  wait :: Int -> m ()

instance MonadWait IO where
  wait = threadDelay
