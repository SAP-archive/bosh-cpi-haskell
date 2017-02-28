{-# LANGUAGE QuasiQuotes #-}

module CPI.Base.AgentConfigSpec(spec) where

import           Test.Hspec

import           CPI.Base.AgentConfig
import           CPI.Base.Data

import qualified Data.ByteString.Char8
import           Data.ByteString.Lazy  (toStrict)
import qualified Data.HashMap.Strict   as HashMap

import           Control.Lens
import           Data.Aeson

import           Data.Aeson.QQ
import           Text.RawString.QQ

spec :: Spec
spec = do
  describe "parseSettings" $ do
    it "should parse settings" $ do
      let input = [r|
      {
        "agent_id": "agent-id",
        "blobstore": {},
        "disks": {
          "system": "/dev/vda",
          "ephemeral": "/var/vcap/data",
          "persistent": {
            "disk1":"/var/vcap/bosh/persistent/disk1"
          }
        },
        "env": {},
        "networks": {},
        "ntp": [],
        "mbus": "mbus",
        "vm": {"name": "vm-name"},
        "trusted_certs": null
      }
      |]
      settings <- parseSettings (Data.ByteString.Char8.pack input)
      settings `shouldBe` AgentSettings {
              _agentId = AgentId "agent-id"
            , _blobstore = Blobstore HashMap.empty
            , _disks = Disks {
                  _system = "/dev/vda"
                , _ephemeral = Just "/var/vcap/data"
                , _persistent = HashMap.singleton "disk1" "/var/vcap/bosh/persistent/disk1"
              }
            , _env = Environment HashMap.empty
            , _networks = HashMap.empty
            , _ntp = []
            , _mbus = "mbus"
            , _vm = Vm {
                _name = "vm-name"
              }
            , _trustedCerts = Nothing
        }
    it "parsing and then rendering should yield the original settings" $ do
      let Success settings = fromJSON [aesonQQ|
        {
          "agent_id": "agent-xxxxxx",
          "blobstore": {
            "provider": "local",
            "options": {
              "endpoint": "http://xx.xx.xx.xx:25250",
              "password": "password",
              "blobstore_path": "/var/vcap/micro_bosh/data/cache",
              "user": "agent"
            }
          },
          "disks": {
            "system": "/dev/xvda",
            "ephemeral": "/dev/sdb",
            "persistent": {}
          },
          "env": {},
          "networks": {
            "default": {
              "type": "manual",
              "ip": "10.234.228.158",
              "netmask": "255.255.255.192",
              "cloud_properties": {"name": "3112 - preprod - back"},
              "dns": [
                "10.234.50.180",
                "10.234.71.124"
              ],
              "gateway": "10.234.228.129",
              "mac": null
            }
          },
          "ntp": [],
          "mbus": "nats://nats:nats-password@yy.yy.yyy:4222",
          "vm": {"name": "vm-yyyy"},
          "trusted_certs": null
        }
        |]
      let settingsSerialized = toStrict $ encode settings
      settingsDeserialized <- parseSettings settingsSerialized
      settingsDeserialized `shouldBe` settings

  describe "addPersistentDisk" $ do
     it "should add a persistent disk" $ do
       let diskSettings = addPersistentDisk defaultSettings "disk1" "/var/vcap/store"
       diskSettings ^. disks.persistent.at "disk1"._Just `shouldBe` "/var/vcap/store"

Success defaultSettings = fromJSON [aesonQQ|
  {
    "agent_id": "agent-xxxxxx",
    "blobstore": {},
    "disks": {
      "system": "/dev/sda",
      "ephemeral": "/dev/sdb",
      "persistent": {}
    },
    "env": {},
    "networks": {},
    "ntp": [],
    "mbus": "nats://nats:nats-password@yy.yy.yyy:4222",
    "vm": {"name": "vm-yyyy"}
  }
  |]
