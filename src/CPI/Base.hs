{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module CPI.Base(
    System(..)
  , CloudError(..)
  , Cpi(..)
  , MonadCpi(..)
  , Request(..)
  , Response(..)
  , ResultType(..)
  , loadConfig
  , readRequest
  , parseRequest
  , writeResponse
  , runRequest
) where

import           Prelude                      hiding (readFile)

import           Control.Applicative
import           Control.Monad.Reader

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString hiding (pack)
import qualified Data.ByteString.Char8        as ByteString (pack)
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (encodeUtf8)

import           Data.Aeson
import           Data.Aeson.Types

import           Data.Typeable

import           System.Environment           (getArgs)

import           Control.Exception            (Exception)
import           Control.Exception.Safe
import           Control.Monad.Trans
import           System.IO                    (stderr)


import           Control.Monad.Log
import           Text.PrettyPrint.Leijen.Text hiding ((<$>))

data CloudError = CloudError String
    deriving (Typeable, Show, Eq)

instance Exception CloudError

class MonadThrow m => System m where
  arguments :: m [Text]
  readFile :: Text -> m ByteString
  readStdin :: m ByteString
  writeStdout :: ByteString -> m ()
  writeStderr :: ByteString -> m ()

instance System IO where
  arguments = getArgs >>= pure . fmap Text.pack
  readFile = ByteString.readFile . Text.unpack
  readStdin = ByteString.getContents
  writeStdout = ByteString.putStr
  writeStderr = liftIO.ByteString.hPutStr stderr

loadConfig :: (Monad m, System m) => m ByteString
loadConfig = do
  args <- arguments
  if not (null args)
    then readFile $ head args
    else throw $ CloudError "No config file location provided"

readRequest :: (Monad m, System m) => m ByteString
readRequest = readStdin

parseRequest :: (Monad m, System m) => ByteString -> m Request
parseRequest raw =
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

data Response =  Response {
    responseResult :: ResultType
} deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response responseResult) =
        object ["result" .= responseResult]
    toEncoding (Response responseResult) =
        pairs ("result" .= responseResult)

data ResultType = Id Text | Boolean Bool deriving (Eq, Show)
instance ToJSON ResultType where
    toJSON (Id text)      = String text
    toJSON (Boolean text) = Bool text

runRequest :: (MonadCpi c m, System m) => (Request -> Cpi c m Response) -> m ()
runRequest handleRequest = do
  config <- loadConfig
            >>= parseConfig
  request <- readRequest
            >>= parseRequest
  response <-
      runCpi (handleRequest request)
        `runReaderT` config
  writeResponse $ toStrict $ encode response

newtype (Monad m, MonadCpi c m, System m) => Cpi c m a = Cpi {
  runCpi :: ReaderT c m a
} deriving (Functor, Applicative, Monad, MonadReader c, MonadThrow, MonadTrans)

instance (System m) => System (Cpi c m) where
  arguments = lift arguments
  readFile = lift.readFile
  readStdin = lift readStdin
  writeStdout = lift.writeStdout
  writeStderr = lift.writeStderr

class (MonadThrow m, MonadLog (WithSeverity Doc) m, System m) => MonadCpi c m | c -> m where
  parseConfig :: ByteString -> m c

instance (Monad m, System m) => MonadLog (WithSeverity Doc) m where
  logMessageFree f = writeStderr $ f (ByteString.pack.show.renderWithSeverity id)
