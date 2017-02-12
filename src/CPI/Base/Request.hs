{-# LANGUAGE OverloadedStrings #-}

module CPI.Base.Request(
    Request(..)
  , StemcellId(..)
  , VolumeId(..)
  , VmId(..)
  , DiskId(..)
  , VmProperties(..)
  , StemcellProperties(..)
  , DiskProperties(..)
  , DiskLocality(..)
  , Environment(..)
  , Networks(..)
  , parseRequest
  , parseArgument
) where

import           CPI.Base.Data
import           CPI.Base.Errors
import           CPI.Base.System


import           Prelude                hiding (readFile)

import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.HashMap.Strict    (HashMap)
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Data.Aeson
import           Data.Aeson.Types

import           Control.Exception.Safe

parseRequest :: (MonadThrow m) => ByteString -> m Request
parseRequest raw =
  either
    (\msg -> throw $ CloudError $ "Could not parse request: '" <> Text.pack msg <> "'")
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

parseArgument :: (MonadThrow m, FromJSON a) => Value -> m a
parseArgument input = case fromJSON input of
  Success a -> return a
  Error msg -> throwM (CloudError $ "Could not parse: '" <> Text.pack msg <> "'")

newtype StemcellId = StemcellId Text deriving (Eq, Show, FromJSON, ToJSON)
newtype VolumeId = VolumeId Text deriving (Eq, Show, FromJSON, ToJSON)
newtype VmId = VmId Text deriving (Eq, Show, FromJSON, ToJSON)
newtype DiskId = DiskId Text deriving (Eq, Show, FromJSON, ToJSON)
newtype VmProperties = VmProperties Value deriving (Eq, Show, FromJSON, ToJSON)
newtype StemcellProperties = StemcellProperties Value deriving (Eq, Show, FromJSON, ToJSON)
newtype DiskProperties = DiskProperties Value deriving (Eq, Show, FromJSON, ToJSON)
type DiskLocality = [VolumeId]
newtype Environment = Environment Value deriving (Eq, Show, FromJSON, ToJSON)
newtype Networks = Networks Value deriving (Eq, Show, FromJSON, ToJSON)
