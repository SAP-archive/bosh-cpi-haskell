{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CPI.BaseSpec(spec) where

import           CPI.Base
import           Test.Hspec

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.Either
import           Data.Maybe

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as ByteString
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)

import           Control.Exception.Safe

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "should load the content of the file given as the first commandline argument" $ do
      let input = TestInput {
              args = ["file"]
            , fileContent = "content"
          }
      runTestResult input loadConfig `shouldBe` "content"
    context "when there is no file specified via commandline argument" $ do
      it "should throw a `CloudError`" $ do
        let input = TestInput {
                args = []
              , fileContent = "content"
            }
        runError input loadConfig `shouldBe` CloudError "No config file location provided"
  describe "readRequest" $ do
    it "should read content from `stdin`" $ do
      let input = TestInput {
            stdinContent = "content"
          }
      runTestResult input readRequest `shouldBe` "content"
  describe "writeResponse" $ do
    it "should write to `stdout`" $ do
      runTestOutput TestInput{} (writeResponse "content") `shouldBe` TestOutput {stdout = "content"}
  describe "parseRequest" $ do
    it "should parse a request with 'method', 'arguments' and 'context' fields" $ do
      runTestResult TestInput{} (parseRequest "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}")
        `shouldBe`
       Request {
           requestMethod = "testMethod"
         , requestArguments = []
         , requestContext = HashMap.empty
       }
  describe "runRequest" $ do
    let input = TestInput {
            args = [""]
          , fileContent = ""
          , stdinContent = "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}"}
    it "provides parsed configuration via reader" $ do
      let input' = input {
            fileContent = "content"
          }
          handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            config <- ask
            if config /= TestConfig "content"
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected configuration " ++ show config
              else pure $ Response ""
      runTestResult input' (runRequest handler) `shouldBe` ()
    it "should read and parse the request" $ do
      let handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            if request /= Request {
                              requestMethod = "testMethod"
                            , requestArguments = []
                            , requestContext = HashMap.empty
                          }
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected request " ++ show request
              else pure $ Response ""
      runTestResult input (runRequest handler) `shouldBe` ()
    it "should write the response" $ do
      let handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            pure $ Response "response"
      runTestOutput input (runRequest handler) `shouldBe` TestOutput "response"

data TestConfig = TestConfig ByteString deriving(Eq, Show)

instance MonadCpi TestConfig TestSystem where
  parseConfig raw = pure $ TestConfig raw

runTest :: TestInput -> TestSystem a -> Either SomeException (a, TestOutput)
runTest input f = runWriterT (runReaderT (runTestSystem f) input)

runTestResult :: TestInput -> TestSystem a -> a
runTestResult input f = case runTest input f of
  Right (a, _) -> a
  Left err     -> error $ "Unexpected result of `runTestResult`: " ++ show err

runTestOutput :: TestInput -> TestSystem a -> TestOutput
runTestOutput input f = case runTest input f of
  Right (_, output) -> output
  Left err     -> error $ "Unexpected result of `runTestOutput`: " ++ show err

runError :: (Show a) => TestInput -> TestSystem a -> CloudError
runError input f = case runTest input f of
  Right output -> error $ "Unexpected result of `runTestResult`: " ++ show output
  Left err     -> fromJust $ fromException err

newtype TestSystem a = TestSystem {
  runTestSystem :: ReaderT TestInput ((WriterT TestOutput) (Either SomeException)) a
} deriving (Functor, Applicative, Monad, MonadReader TestInput, MonadWriter TestOutput, MonadThrow)

data TestInput = TestInput {
    args         :: [Text]
  , fileContent  :: ByteString
  , stdinContent :: ByteString
}

data TestOutput = TestOutput {
    stdout :: ByteString
} deriving (Eq, Show)

instance Monoid TestOutput where
  mempty = TestOutput ByteString.empty
  mappend (TestOutput left) (TestOutput right) = TestOutput $ mappend left right

instance System TestSystem where
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
  writeStdout = tell.TestOutput
