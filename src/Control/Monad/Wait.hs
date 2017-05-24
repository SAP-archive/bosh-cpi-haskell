{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Wait(
    MonadWait(..)
  , Timeout(..)
  , WaitConfig(..)
  , Retry(..)
  , waitFor
) where

import           Control.Concurrent     (threadDelay)
import           Control.Exception.Safe
import           Control.Monad.Trans

import           Data.Hourglass
import           Data.Maybe

class (Monad m, MonadThrow m) => MonadWait m where
  wait :: (TimeInterval i) => i -> m ()

instance MonadWait IO where
  wait n = threadDelay $ 1000000 * (fromIntegral . toInteger . toSeconds) n

data TimeInterval i => WaitConfig i = WaitConfig {
    retries  :: Retry
  , interval :: i
}

data Retry =
    Unlimited
  | Retry {
      count :: Int
    }

instance Eq Retry where
  Unlimited == Unlimited = True
  (Retry n) == (Retry m) = n == m

data Timeout = Timeout deriving (Typeable, Show, Eq)

instance Exception Timeout

waitFor :: (MonadWait m, MonadThrow m, TimeInterval i) => WaitConfig i -> m (Maybe a) -> (a -> Bool) -> m a
waitFor waitConfig next predicate =
  go waitConfig 0
    where
      go waitConfig n = do
        mResource <- next
        if isJust mResource && predicate (fromJust mResource)
          then pure (fromJust mResource)
          else
            case retries waitConfig of
              Retry m | m < n -> throwM Timeout
              _ -> do
                wait $ interval waitConfig
                go waitConfig (n + 1)
