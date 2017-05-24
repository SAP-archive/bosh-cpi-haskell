module Control.Monad.Time(
    MonadTime(..)
) where

import           Time.System
import           Time.Types

class (Monad m) => MonadTime m where
  currentTime :: m Elapsed

instance MonadTime IO where
  currentTime = timeCurrent
