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

import           Control.Exception
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

run :: Input -> SystemT a -> Either SomeException (a, Output)
run input f = runWriterT (runReaderT (runSystemT f) input)

runResult :: Input -> SystemT a -> a
runResult input f = case run input f of
  Right (a, _) -> a
  Left err     -> error $ "Unexpected result of `runResult`: " ++ show err

runError :: (Show a) => Input -> SystemT a -> CloudError
runError input f = case run input f of
  Right output -> error $ "Unexpected result of `runResult`: " ++ show output
  Left err     -> fromJust $ fromException err

newtype SystemT a = SystemT {
  runSystemT :: ReaderT Input ((WriterT Output) (Either SomeException)) a
} deriving (Functor, Applicative, Monad, MonadReader Input, MonadWriter Output, MonadThrow)

data Input = Input {
    args        :: [Text]
  , fileContent :: ByteString
}

data Output = Output {
    stdout :: ByteString
} deriving (Eq, Show)

instance Monoid Output where
  mempty = Output ByteString.empty
  mappend (Output left) (Output right) = Output $ mappend left right

instance System SystemT where
  arguments = args <$> ask
  readFile path = do
    input <- ask
    let expectedPath = (head.args) input
    if path == expectedPath
      then pure $ fileContent input
      else error "Unexpected argument for function call to `readFile`"
