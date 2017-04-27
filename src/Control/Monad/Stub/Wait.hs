module Control.Monad.Stub.Wait(
    HasWaitCount(..)
) where

import           Control.Exception.Safe
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Wait
import           Control.Monad.Writer

class (Monoid a) => HasWaitCount a where
  asWaitCount :: Int -> a

instance (Monad m, MonadThrow m, Monoid w, HasWaitCount w) => MonadWait (StubT r s w m) where
  wait n = tell $ asWaitCount n
