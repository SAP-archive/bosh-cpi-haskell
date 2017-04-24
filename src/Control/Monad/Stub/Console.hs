module Control.Monad.Stub.Console(
    HasStdin(..)
  , HasStdout(..)
  , HasStderr(..)
) where

import           Control.Monad.Stub.StubMonad

import           Data.ByteString              (ByteString)

import           Control.Monad.Console
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

class HasStdin a where
  asStdin :: a -> ByteString

class (Monoid a) => HasStdout a where
  asStdout :: ByteString -> a

class (Monoid a) => HasStderr a where
  asStderr :: ByteString -> a

instance (Monad m, HasStdin c, HasStdout w, HasStderr w) => MonadConsole (StubT c s w m) where
  readStdin = asks asStdin
  writeStdout s = tell $ asStdout s
  writeStderr s = tell $ asStderr s
