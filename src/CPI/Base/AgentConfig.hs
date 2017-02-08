{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module CPI.Base.AgentConfig(
    AgentSettings(..)
  , AgentId(..)
  , agentId
  , blobstore
  , disks
  , env
  , networks
  , ntp
  , mbus
  , vm
  , Blobstore(..)
  , Disks(..)
  , system
  , ephemeral
  , persistent
  , Network(..)
  , Vm(..)
  , name
  , trustedCerts
  , parseSettings
  , addPersistentDisk
) where

import           CPI.Base.Errors

import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.HashMap.Strict    (HashMap)
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as Text

parseSettings :: (MonadThrow m) => ByteString -> m AgentSettings
parseSettings raw =
  either
    (\msg -> throw $ CloudError $ "Could not parse agent settings " <> Text.pack msg)
    return
    (eitherDecode' $ fromStrict raw)

data AgentSettings = AgentSettings {
    _agentId      :: AgentId
  , _blobstore    :: Blobstore
  , _disks        :: Maybe Disks
  , _env          :: HashMap Text Text
  , _networks     :: HashMap Text Network
  , _ntp          :: [Text]
  , _mbus         :: Text
  , _vm           :: Vm
  , _trustedCerts :: Maybe Text
} deriving (Eq, Show)

newtype AgentId = AgentId Text
    deriving (Eq, Show)
newtype Blobstore = Blobstore (HashMap Text Value)
    deriving (Eq, Show)
data Disks = Disks {
    _system     :: Text
  , _ephemeral  :: Maybe Text
  , _persistent :: HashMap Text Text
} deriving (Eq, Show)
newtype Network = Network (HashMap Text Value)
    deriving (Eq, Show)
data Vm = Vm {
  _name :: Text
} deriving (Eq, Show)

makeLenses ''AgentSettings
makeLenses ''AgentId
makeLenses ''Blobstore
makeLenses ''Disks
makeLenses ''Network
makeLenses ''Vm

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AgentSettings)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AgentId)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Blobstore)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Disks)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Network)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Vm)

addPersistentDisk :: AgentSettings -> Text -> Text -> AgentSettings
addPersistentDisk settings diskId path = settings & disks._Just.persistent.at diskId.non "" .~ path
  -- Example settings.json
  -- {
  --   "agent_id": "agent-xxxxxx",
  --   "blobstore": {
  --     "provider": "local",
  --     "options": {
  --       "endpoint": "http://xx.xx.xx.xx:25250",
  --       "password": "password",
  --       "blobstore_path": "/var/vcap/micro_bosh/data/cache",
  --       "user": "agent"
  --     }
  --   },
  --   "disks": {
  --     "system": "/dev/xvda",
  --     "ephemeral": "/dev/sdb",
  --     "persistent": {}
  --   },
  --   "env": {},
  --   "networks": {
  --     "default": {
  --       "type": "manual",
  --       "ip": "10.234.228.158",
  --       "netmask": "255.255.255.192",
  --       "cloud_properties": {"name": "3112 - preprod - back"},
  --       "dns": [
  --         "10.234.50.180",
  --         "10.234.71.124"
  --       ],
  --       "gateway": "10.234.228.129",
  --       "mac": null
  --     }
  --   },
  --   "ntp": [],
  --   "mbus": "nats://nats:nats-password@yy.yy.yyy:4222",
  --   "vm": {"name": "vm-yyyy"},
  --   "trusted_certs": null
  -- }


  -- makeLenses ''PublicKeys
  -- instance ToJSON PublicKeys where
  --   toJSON (PublicKeys keys) =
  --       object ["public-keys" .= fmap createEntry indexedKeys]
  --       where
  --         indexedKeys :: [(Int,Text)]
  --         indexedKeys = zip [0..(length keys)] keys
  --         createEntry :: (Int, Text) -> Value
  --         createEntry (index, key) = object [(Text.pack . show) index .= object ["openssh-key" .= key]]
