module Control.Monad.Stub.Environment(
    HasEnvironment(..)
) where

import           Control.Monad.Environment
import           Control.Monad.Stub.StubMonad

import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasEnvironment a where
  asEnvironment :: a -> HashMap Text Text
  asEnvironment = const HashMap.empty

instance (Monad m, MonadThrow m, Monoid w, HasEnvironment s) => MonadEnvironment (StubT c s w m) where
  environment = gets asEnvironment
