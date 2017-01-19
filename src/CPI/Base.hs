{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module CPI.Base(
    System(..)
  , CloudError(..)
  , Cpi(..)
  , MonadCpi(..)
  , Request(..)
  , Response(..)
  , loadConfig
  , readRequest
  , parseRequest
  , writeResponse
  , runRequest
) where

import           Prelude                hiding (readFile)

import           Control.Applicative
import           Control.Monad.Reader

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import           Data.ByteString.Lazy   (fromStrict)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Data.Aeson
import           Data.Aeson.Types

import           Data.Typeable

import           System.Environment     (getArgs)

import           Control.Exception      (Exception)
import           Control.Exception.Safe

data CloudError = CloudError String
    deriving (Typeable, Show, Eq)

instance Exception CloudError

class MonadThrow m => System m where
  arguments :: m [Text]
  readFile :: Text -> m ByteString
  readStdin :: m ByteString
  writeStdout :: ByteString -> m ()

instance System IO where
  arguments = getArgs >>= pure . fmap Text.pack
  readFile = ByteString.readFile . Text.unpack
  readStdin = ByteString.getContents
  writeStdout = ByteString.putStr

loadConfig :: (Monad m, System m) => m ByteString
loadConfig = do
  args <- arguments
  if not (null args)
    then readFile $ head args
    else throw $ CloudError "No config file location provided"

readRequest :: (Monad m, System m) => m ByteString
readRequest = readStdin

parseRequest :: (Monad m, System m) => ByteString -> m Request
parseRequest raw = do
  either
    (\msg -> throw $ CloudError $ "Could not parse request" ++ msg)
    return
    (eitherDecode' $ fromStrict raw)

writeResponse :: (Monad m, System m) => ByteString -> m ()
writeResponse = writeStdout

data Request = Request {
    requestMethod    :: Text
  , requestArguments :: [Value]
  , requestContext   :: HashMap Text Value
} deriving (Eq, Show)

instance FromJSON Request where
  parseJSON (Object v) = Request
                        <$> v .: "method"
                        <*> v .: "arguments"
                        <*> v .: "context"
  parseJSON invalid    = typeMismatch "Request" invalid

data Response = Response ByteString deriving (Eq, Show)

runRequest :: (MonadCpi c m, System m) => (Request -> Cpi c m Response) -> m ()
runRequest handleRequest = do
  config <- loadConfig
            >>= parseConfig
  request <- readRequest
            >>= parseRequest
  (Response response) <-
    runReaderT (
      runCpi (
        handleRequest request
        ))
    config
  writeResponse response

newtype (Monad m, MonadCpi c m) => Cpi c m a = Cpi {
  runCpi :: ReaderT c m a
} deriving (Functor, Applicative, Monad, MonadReader c, MonadThrow)

class (MonadThrow m) => MonadCpi c m | c -> m where
  parseConfig :: ByteString -> m c
