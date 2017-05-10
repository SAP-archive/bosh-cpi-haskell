module Control.Monad.Stub.Console(
    HasStdin(..)
  , HasStdout(..)
  , HasStderr(..)
) where

import           Control.Monad.Stub.StubMonad

import           Data.ByteString              (ByteString)

import           Control.Exception.Safe
import           Control.Monad.Console
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasStdin a where
  asStdin :: a -> ByteString
  asStdin = const ""

class (Monoid a) => HasStdout a where
  asStdout :: ByteString -> a
  asStdout = mempty

class (Monoid a) => HasStderr a where
  asStderr :: ByteString -> a
  asStderr = mempty

instance (Monad m, MonadThrow m, HasStdin c, HasStdout w, HasStderr w) => MonadConsole (StubT c s w m) where
  readStdin = asks asStdin
  writeStdout s = tell $ asStdout s
  writeStderr s = tell $ asStderr s
