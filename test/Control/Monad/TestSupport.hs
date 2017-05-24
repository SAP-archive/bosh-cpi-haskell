{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.TestSupport(
    TestInput(..)
  , emptyTestInput
  , TestOutput(..)
  , emptyTestOutput
  , TestState(..)
  , emptyTestState
) where

import           CPI.Base


import           Control.Monad.Stub.Arguments
import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Stub.Time
import           Control.Monad.Stub.Wait

import           Control.Monad.Reader
import           Control.Monad.Time
import           Control.Monad.Wait
import           Control.Monad.Writer
import           Data.Maybe
import           Data.Monoid

import           Control.Exception.Safe
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as ByteString hiding (unpack)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hourglass
import           Data.Text                     (Text)
import           Test.Hspec

emptyTestState :: TestState
emptyTestState = TestState {
    fileSystem = HashMap.empty
  , elapsed = 0
  , events = HashMap.empty
}

data TestState = TestState {
    fileSystem :: HashMap Text ByteString
  , elapsed    :: Elapsed
  , events     :: HashMap Elapsed [TestState -> TestState]
}

instance HasFiles TestState where
  asFiles = fileSystem

instance HasTime TestState where
  asTime = elapsed
  updateTime s elapsed = s {
    elapsed = elapsed
  }

instance HasTimeline TestState where
  asTimeline = events
  updateTimeline s events = s {
    events = events
  }

emptyTestInput = TestInput {
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

emptyTestOutput = TestOutput {
    stdout = ""
  , stderr = ""
  , waitCount = []
}

data TestOutput = TestOutput {
    stdout    :: ByteString
  , stderr    :: ByteString
  , waitCount :: [Seconds]
} deriving (Eq, Show)

instance Monoid TestOutput where
  mempty = emptyTestOutput
  mappend left right = emptyTestOutput {
      stdout = stdout left <> stdout right
    , stderr = stderr left <> stderr right
    , waitCount = waitCount left <> waitCount right
  }

instance HasStdout TestOutput where
  asStdout out = emptyTestOutput {
    stdout = out
  }

instance HasStderr TestOutput where
  asStderr err = emptyTestOutput {
    stderr = err
  }

instance HasWaitCount TestOutput where
  asWaitCount n = emptyTestOutput {
    waitCount = [toSeconds n]
  }
