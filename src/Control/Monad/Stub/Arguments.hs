module Control.Monad.Stub.Arguments(
    HasArguments(..)
) where

import           Control.Monad.Arguments
import           Control.Monad.Stub.StubMonad

import           Data.Text               (Text)

import           Control.Monad.Console
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasArguments a where
  asArguments :: a -> [Text]

instance (Monad m, Monoid w, HasArguments c) => MonadArguments (StubT c s w m) where
  arguments = asks asArguments
