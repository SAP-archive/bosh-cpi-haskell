{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Base.Data(
    AgentId(..)
  , Network(..)
  , Networks(..)
  , Environment(..)
  , Blobstore(..)
) where

import           Control.Lens
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)

newtype AgentId = AgentId Text
    deriving (Eq, Show, FromJSON, ToJSON)

newtype Network = Network (HashMap Text Value)
    deriving (Eq, Show, FromJSON, ToJSON)

newtype Networks = Networks (HashMap Text Network)
    deriving (Eq, Show, FromJSON, ToJSON)

newtype Environment = Environment (HashMap Text Value)
    deriving (Eq, Show, FromJSON, ToJSON)

newtype Blobstore = Blobstore (HashMap Text Value)
    deriving (Eq, Show, FromJSON, ToJSON)

makeWrapped ''AgentId
makeWrapped ''Network
makeWrapped ''Networks
makeWrapped ''Environment
makeWrapped ''Blobstore
