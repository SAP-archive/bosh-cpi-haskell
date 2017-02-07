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
      let input = mkTestInput {
              args = ["file"]
            , fileContent = "content"
          }
      result <- runTestResult input loadConfig
      result `shouldBe` "content"
    context "when there is no file specified via commandline argument" $ do
      it "should throw a `CloudError`" $ do
        let input = mkTestInput {
                args = []
              , fileContent = "content"
            }
        result <- runError input loadConfig
        result `shouldBe` CloudError "No config file location provided"
  describe "readRequest" $ do
    it "should read content from `stdin`" $ do
      let input = mkTestInput {
            stdinContent = "content"
          }
      result <- runTestResult input readRequest
      result `shouldBe` "content"
  describe "writeResponse" $ do
    it "should write to `stdout`" $ do
      result <- runTestOutput mkTestInput (writeResponse "content")
      result `shouldBe` mkTestOutput {stdout = "content"}
  describe "parseRequest" $ do
    it "should parse a request with 'method', 'arguments' and 'context' fields" $ do
      result <- runTestResult mkTestInput (parseRequest "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}")
      result `shouldBe` Request {
           requestMethod = "testMethod"
         , requestArguments = []
         , requestContext = HashMap.empty
       }
  describe "runRequest" $ do
    let input = mkTestInput {
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
              else pure Response {
                responseResult = Id "id"
              }
      result <- runTestResult input' (runRequest handler)
      result `shouldBe` ()
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
              else pure Response {
                responseResult = Id "id"
              }
      result <- runTestResult input (runRequest handler)
      result `shouldBe` ()
    context "when the result type is a string" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig TestSystem Response
            handler request =
              pure Response {
                responseResult = Id "id"
              }
        result <- runTestOutput input (runRequest handler)
        stdout result `shouldBe` "{\"result\":\"id\"}"
    context "when the result type is a boolean" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig TestSystem Response
            handler request =
              pure Response {
                responseResult = Boolean True
              }
        result <- runTestOutput input (runRequest handler)
        stdout result `shouldBe` "{\"result\":true}"

data TestConfig = TestConfig ByteString deriving(Eq, Show)

instance MonadCpi TestConfig TestSystem where
  parseConfig raw = pure $ TestConfig raw

data TestCpi

runTest :: TestInput -> TestSystem a -> IO (a, TestOutput)
runTest input f = runWriterT (runReaderT (runTestSystem f) input)

runTestResult :: TestInput -> TestSystem a -> IO a
runTestResult input f = do
  (a, _) <- runTest input f
  pure a

runTestOutput :: TestInput -> TestSystem a -> IO TestOutput
runTestOutput input f = do
  (_, output) <- runTest input f
  pure output

runError :: (Show a) => TestInput -> TestSystem a -> IO CloudError
runError input f = do
  result <- try( runTest input f )
  pure $ case result of
    Right output -> error $ "Unexpected result of `runTestResult`: " ++ show output
    Left err     -> fromJust $ fromException err

newtype TestSystem a = TestSystem {
  runTestSystem :: ReaderT TestInput ((WriterT TestOutput) IO) a
} deriving (Functor, Applicative, Monad, MonadReader TestInput, MonadWriter TestOutput, MonadThrow, MonadIO)

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
} deriving (Eq, Show)

mkTestOutput = TestOutput {
    stdout = ""
}

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
