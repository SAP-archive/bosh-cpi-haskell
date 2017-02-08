module CPI.Base.Request(
    Request(..)
  , parseRequest
) where

import           CPI.Base.Errors
import           CPI.Base.System


import           Prelude                hiding (readFile)

import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.HashMap.Strict    (HashMap)
import           Data.Text              (Text)

import           Data.Aeson
import           Data.Aeson.Types

import           Control.Exception.Safe

parseRequest :: (System m) => ByteString -> m Request
parseRequest raw =
  either
    (\msg -> throw $ CloudError $ "Could not parse request" ++ msg)
    return
    (eitherDecode' $ fromStrict raw)

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
