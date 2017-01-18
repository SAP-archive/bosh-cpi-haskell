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
import           Data.Text              (Text)
import qualified Data.Text              as Text

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
parseRequest raw = pure $ Request raw

writeResponse :: (Monad m, System m) => ByteString -> m ()
writeResponse = writeStdout

data Request = Request ByteString deriving (Eq, Show)
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
