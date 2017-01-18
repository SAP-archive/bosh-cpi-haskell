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
import           Data.Text                  (Text)

import           Control.Exception.Safe

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "should load the content of the file given as the first commandline argument" $ do
      let input = Input {
              args = ["file"]
            , fileContent = "content"
          }
      runResult input loadConfig `shouldBe` "content"
    context "when there is no file specified via commandline argument" $ do
      it "should throw a `CloudError`" $ do
        let input = Input {
                args = []
              , fileContent = "content"
            }
        runError input loadConfig `shouldBe` CloudError "No config file location provided"
  describe "readRequest" $ do
    it "should read content from `stdin`" $ do
      let input = Input {
            stdinContent = "content"
          }
      runResult input readRequest `shouldBe` "content"
  describe "writeResponse" $ do
    it "should write to `stdout`" $ do
      runOutput Input{} (writeResponse "content") `shouldBe` Output {stdout = "content"}
  describe "parseRequest" $ do
    it "should " $ do
      runResult Input{} (parseRequest "request") `shouldBe` Request "request"
  describe "runRequest" $ do
    it "provides parsed configuration via reader" $ do
      let input = Input {
              args = [""]
            , fileContent = "content"
            , stdinContent = ""}
          handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            config <- ask
            if config /= TestConfig "content"
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected configuration " ++ show config
              else pure $ Response ""
      runResult input (runRequest handler) `shouldBe` ()
    it "should read and parse the request" $ do
      let input = Input {
              args = [""]
            , fileContent = ""
            , stdinContent = "request"}
          handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            if request /= Request "request"
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected request " ++ show request
              else pure $ Response ""
      runResult input (runRequest handler) `shouldBe` ()
    it "should write the response" $ do
      let input = Input {
              args = [""]
            , fileContent = ""
            , stdinContent = ""}
          handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            pure $ Response "response"
      runOutput input (runRequest handler) `shouldBe` Output "response"

data TestConfig = TestConfig ByteString deriving(Eq, Show)

instance MonadCpi TestConfig TestSystem where
  parseConfig raw = pure $ TestConfig raw

run :: Input -> TestSystem a -> Either SomeException (a, Output)
run input f = runWriterT (runReaderT (runTestSystem f) input)

runResult :: Input -> TestSystem a -> a
runResult input f = case run input f of
  Right (a, _) -> a
  Left err     -> error $ "Unexpected result of `runResult`: " ++ show err

runOutput :: Input -> TestSystem a -> Output
runOutput input f = case run input f of
  Right (_, output) -> output
  Left err     -> error $ "Unexpected result of `runResult`: " ++ show err

runError :: (Show a) => Input -> TestSystem a -> CloudError
runError input f = case run input f of
  Right output -> error $ "Unexpected result of `runResult`: " ++ show output
  Left err     -> fromJust $ fromException err

newtype TestSystem a = TestSystem {
  runTestSystem :: ReaderT Input ((WriterT Output) (Either SomeException)) a
} deriving (Functor, Applicative, Monad, MonadReader Input, MonadWriter Output, MonadThrow)

data Input = Input {
    args         :: [Text]
  , fileContent  :: ByteString
  , stdinContent :: ByteString
}

data Output = Output {
    stdout :: ByteString
} deriving (Eq, Show)

instance Monoid Output where
  mempty = Output ByteString.empty
  mappend (Output left) (Output right) = Output $ mappend left right

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
  writeStdout = tell.Output
