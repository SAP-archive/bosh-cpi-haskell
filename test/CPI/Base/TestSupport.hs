{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CPI.Base.TestSupport(
    TestConfig(..)
  , TestInput(..)
  , mkTestInput
  , TestOutput(..)
  , mkTestOutput
  , TestSystem
  , runTest
  , runTestResult
  , runTestOutput
  , runError
  , anyCloudError
  , cloudErrorWithMessage
) where

import           CPI.Base

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Maybe

import           Control.Exception.Safe
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString hiding (unpack)
import           Data.Text              (Text)
import           Test.Hspec

data TestConfig = TestConfig ByteString deriving(Eq, Show)

instance MonadCpi TestConfig (TestSystem TestInput TestOutput) where
  parseConfig raw = pure $ TestConfig raw

runTest :: input -> (TestSystem input output) a -> IO (a, output)
runTest input f = runWriterT (runReaderT (runTestSystem f) input)

runTestResult :: input -> (TestSystem input output) a -> IO a
runTestResult input f = do
  (a, _) <- runTest input f
  pure a

runTestOutput :: input -> (TestSystem input output) a -> IO output
runTestOutput input f = do
  (_, output) <- runTest input f
  pure output

runError :: (Show a, Show output) => input -> (TestSystem input output) a -> IO CloudError
runError input f = do
  result <- try( runTest input f )
  pure $ case result of
    Right output -> error $ "Unexpected result of `runTestResult`: " ++ show output
    Left err     -> fromJust $ fromException err

newtype TestSystem input output a = TestSystem {
  runTestSystem :: ReaderT input ((WriterT output) IO) a
} deriving (Functor, Applicative, Monad, MonadReader input, MonadWriter output, MonadThrow, MonadCatch, MonadIO)

data TestInput = TestInput {
    args         :: [Text]
  , fileContent  :: ByteString
  , stdinContent :: ByteString
}

mkTestInput = TestInput {
    args = []
  , fileContent = ""
  , stdinContent = ""
}

data TestOutput = TestOutput {
    stdout :: ByteString
  , stderr :: ByteString
} deriving (Eq, Show)

mkTestOutput = TestOutput {
    stdout = ""
  , stderr = ""
}

instance Monoid TestOutput where
  mempty = TestOutput ByteString.empty ByteString.empty
  mappend (TestOutput leftStdout leftStderr) (TestOutput rightStdout rightStderr) = TestOutput (leftStdout `mappend` rightStdout) (leftStderr `mappend` rightStderr)

instance System (TestSystem TestInput TestOutput) where
  arguments = args <$> ask
  readFile path = do
    input <- ask
    let expectedPath = (head.args) input
    if path == expectedPath
      then pure $ fileContent input
      else error "Unexpected argument for function call to `readFile`"
  readStdin = do
    input <- ask
    pure $ stdinContent input
  writeStdout output = tell $ mkTestOutput {stdout = output}
  writeStderr output = tell $ mkTestOutput {stderr = output}

anyCloudError :: Selector CloudError
anyCloudError = const True

cloudErrorWithMessage :: Text -> Selector CloudError
cloudErrorWithMessage expectedMessage (CloudError message) = expectedMessage == message
