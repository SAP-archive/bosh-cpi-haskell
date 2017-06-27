{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module CPI.Base.AgentConfig(
    AgentSettings(..)
  , agentId
  , blobstore
  , disks
  , env
  , networks
  , ntp
  , mbus
  , vm
  , Disks(..)
  , system
  , ephemeral
  , persistent
  , Network(..)
  , Vm(..)
  , name
  , trustedCerts
  , parseSettings
  , initialAgentSettings
  , parseValue
  , addPersistentDisk
  , removePersistentDisk
) where

import           CPI.Base.Data
import           CPI.Base.Errors

import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as Text

data AgentSettings = AgentSettings {
    _agentId      :: AgentId
  , _blobstore    :: Maybe Blobstore
  , _disks        :: Disks
  , _env          :: Environment
  , _networks     :: HashMap Text Network
  , _ntp          :: [Text]
  , _mbus         :: Text
  , _vm           :: Vm
  , _trustedCerts :: Maybe Text
} deriving (Eq, Show)

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
makeLenses ''Disks
makeLenses ''Network
makeLenses ''Vm

$(deriveJSON defaultOptions{fieldLabelModifier = \label -> if label == "_agentId" then "agent_id" else drop 1 label} ''AgentSettings)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Disks)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Network)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Vm)


parseSettings :: (MonadThrow m) => ByteString -> m AgentSettings
parseSettings raw =
  either
    (\msg -> throw $ CloudError $ "Could not parse agent settings " <> Text.pack msg)
    return
    (eitherDecode' $ fromStrict raw)

initialAgentSettings :: AgentId -> Maybe Blobstore -> Environment -> [Text] -> Text -> AgentSettings
initialAgentSettings agentId blobstore env ntp mbus =
  AgentSettings {
      _agentId = agentId
    , _blobstore = blobstore
    , _disks = Disks {
        _system = "/dev/sda"
      , _ephemeral = Nothing
      , _persistent = HashMap.empty
    }
    , _env = env
    , _networks = HashMap.empty
    , _ntp = ntp
    , _mbus = mbus
    , _vm = Vm $ Unwrapped agentId
    , _trustedCerts = Nothing
  }

parseValue :: (FromJSON a, MonadThrow m) => Text -> Value -> m a
parseValue name raw = case fromJSON raw of
  Success value -> pure value
  Error msg -> throwM $ CloudError $ "Could not parse '" <> name <> "': " <> Text.pack msg

-- TODO we should use DiskId instead of Text
addPersistentDisk :: AgentSettings -> Text -> Text -> AgentSettings
addPersistentDisk settings diskId path = settings & disks.persistent.at diskId.non "" .~ path

-- TODO we should use DiskId instead of Text
removePersistentDisk :: AgentSettings -> Text -> AgentSettings
removePersistentDisk settings diskId = settings & disks.persistent.at diskId .~ Nothing
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
