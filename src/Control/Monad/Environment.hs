module Control.Monad.Environment(
    MonadEnvironment(..)
) where

import           Control.Exception.Safe
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           System.Environment

class (Monad m, MonadThrow m) => MonadEnvironment m where
  environment :: m (HashMap Text Text)

instance MonadEnvironment IO where
  environment = getEnvironment >>= pure . HashMap.fromList . fmap (\(k,v) -> (Text.pack k, Text.pack v))
