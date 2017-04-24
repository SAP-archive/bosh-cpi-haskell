{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CPI.Base.TestSupport(
    DummyException(..)
  , TestConfig(..)
  , TestInput(..)
  , mkTestInput
  , TestOutput(..)
  , mkTestOutput
  , TestState(..)
  , mkTestState
  , anyCloudError
  , cloudErrorWithMessage
) where

import           CPI.Base


import           Control.Monad.Stub.Arguments
import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Maybe
import           Data.Monoid

import           Control.Exception.Safe
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as ByteString hiding (unpack)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                     (Text)
import           Test.Hspec


data DummyException = DummyException Text
  deriving (Eq, Show, Typeable)

instance Exception DummyException

data TestConfig = TestConfig ByteString deriving(Eq, Show)

mkTestState :: TestState
mkTestState = TestState {
    fileSystem = HashMap.empty
}

data TestState = TestState {
    fileSystem :: HashMap Text ByteString
} deriving (Eq, Show)

instance HasFiles TestState where
  asFiles = fileSystem

mkTestInput = TestInput {
    args = []
  , stdinContent = ""
}

data TestInput = TestInput {
    args         :: [Text]
  , stdinContent :: ByteString
} deriving (Eq, Show)

instance HasArguments TestInput where
  asArguments = args

instance HasStdin TestInput where
  asStdin = stdinContent

mkTestOutput = TestOutput {
    stdout = ""
  , stderr = ""
}

data TestOutput = TestOutput {
    stdout :: ByteString
  , stderr :: ByteString
} deriving (Eq, Show)

instance HasStdout TestOutput where
  asStdout out = mkTestOutput {
    stdout = out
  }

instance HasStderr TestOutput where
  asStderr err = mkTestOutput {
    stderr = err
  }

instance Monoid TestOutput where
  mempty = TestOutput ByteString.empty ByteString.empty
  mappend (TestOutput leftStdout leftStderr) (TestOutput rightStdout rightStderr) = TestOutput (leftStdout `mappend` rightStdout) (leftStderr `mappend` rightStderr)

anyCloudError :: Selector CloudError
anyCloudError = const True

cloudErrorWithMessage :: Text -> Selector CloudError
cloudErrorWithMessage expectedMessage (CloudError message) = expectedMessage == message
