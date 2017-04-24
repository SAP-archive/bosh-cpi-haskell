module Control.Monad.Arguments(
    MonadArguments(..)
) where

import           Data.Text          (Text)
import           Data.Text          as Text
import           System.Environment

class (Monad m) => MonadArguments m where
  arguments :: m [Text]

instance MonadArguments IO where
  arguments = getArgs >>= pure . fmap Text.pack
