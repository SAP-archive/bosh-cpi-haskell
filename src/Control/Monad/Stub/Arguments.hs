module Control.Monad.Stub.Arguments(
    HasArguments(..)
) where

import           Control.Monad.Arguments
import           Control.Monad.Stub.StubMonad

import           Data.Text                    (Text)

import           Control.Exception.Safe
import           Control.Monad.Console
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasArguments a where
  asArguments :: a -> [Text]
  asArguments = const []

instance (Monad m, MonadThrow m, Monoid w, HasArguments c) => MonadArguments (StubT c s w m) where
  arguments = asks asArguments
