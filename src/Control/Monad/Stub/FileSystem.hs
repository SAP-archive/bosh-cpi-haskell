module Control.Monad.Stub.FileSystem(
    HasFiles(..)
) where

import           Control.Monad.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe

import           Control.Exception.Safe
import           Data.ByteString              (ByteString)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)

class HasFiles a where
  asFiles :: a -> HashMap Text ByteString

instance (Monad m, MonadThrow m, HasFiles s, Monoid w) => MonadFileSystem (StubT c s w m) where
  readFile path = do
    files <- gets asFiles
    --TODO *** Exception: <path>: openFile: does not exist (No such file or directory)
    pure $ fromJust $ path `HashMap.lookup` files
