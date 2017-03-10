{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CPI.BaseSpec(spec) where

import           CPI.Base
import           CPI.Base.TestSupport
import           Test.Hspec

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.Either
import           Data.Maybe

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as ByteString hiding (unpack)
import qualified Data.ByteString.Char8      as ByteString
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Vector                as Vector

import           Data.Aeson.QQ

import           Control.Monad.Log

import           Control.Exception.Safe

spec :: Spec
spec = do
  describe "runRequest" $ do
    let input = mkTestInput {
            args = [""]
          , fileContent = ""
          , stdinContent = "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}"}
    it "provides parsed configuration via reader" $ do
      let input' = input {
            fileContent = "content"
          }
          handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
          handler request = do
            config <- ask
            if config /= TestConfig "content"
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ ("Unexpected configuration " <> (Text.pack (show config)))
              else pure $ createSuccess $ Id "id"
      result <- runTestResult input' (runRequest handler)
      result `shouldBe` ()
    it "should read and parse the request" $ do
      let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
          handler request = do
            if request /= Request {
                              requestMethod = "testMethod"
                            , requestArguments = []
                            , requestContext = HashMap.empty
                          }
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ ("Unexpected request " <> (Text.pack $ show request))
              else pure $ createSuccess $ Id "id"
      result <- runTestResult input (runRequest handler)
      result `shouldBe` ()
    context "when the result type is a string" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
            handler request =
              pure $ createSuccess $ Id "id"
        result <- runTestOutput input (runRequest handler)
        (eitherDecode'.fromStrict.stdout) result `shouldBe` Right [aesonQQ|{"result":"id", error:null, "log":""}|]
    context "when the result type is a boolean" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
            handler request =
              pure $ createSuccess $ Boolean True
        result <- runTestOutput input (runRequest handler)
        (eitherDecode'.fromStrict.stdout) result `shouldBe` Right [aesonQQ|{"result":true, error:null, "log":""}|]
    context "when an exception is thrown" $ do
      it "should write an error response" $ do
        let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
            handler request = throwM $ DummyException "BOOM!!!"
        result <- runTestOutput input (runRequest handler)
        (eitherDecode'.fromStrict.stdout) result `shouldBe` Right [aesonQQ|
          {
            "result" : null,
            "error" : {
              "type" : "Bosh::Clouds::CloudError",
              "message" : "Unknown error: 'DummyException \"BOOM!!!\"'",
              "ok_to_retry" : false
            },
            "log" : ""
          }|]
    context "when a CloudError is thrown" $ do
      it "should write an error response" $ do
        let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
            handler request = throwM $ CloudError "BOOM!!!"
        result <- runTestOutput input (runRequest handler)
        (eitherDecode'.fromStrict.stdout) result `shouldBe` Right [aesonQQ|
          {
            "result" : null,
            "error" : {
              "type" : "Bosh::Clouds::CloudError",
              "message" : "BOOM!!!",
              "ok_to_retry" : false
            },
            "log" : ""
          }|]
    context "when a NotImplemented is thrown" $ do
      it "should write an error response" $ do
        let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
            handler request = throwM $ NotImplemented "BOOM!!!"
        result <- runTestOutput input (runRequest handler)
        (eitherDecode'.fromStrict.stdout) result `shouldBe` Right [aesonQQ|
          {
            "result" : null,
            "error" : {
              "type" : "Bosh::Clouds::NotImplemented",
              "message" : "BOOM!!!",
              "ok_to_retry" : false
            },
            "log" : ""
          }|]
    it "provides logging facilities" $ do
      let handler :: Request -> Cpi TestConfig (TestSystem TestInput TestOutput) Response
          handler request = do
            logDebug "test debug message"
            pure $ createSuccess $ Id "id"
      result <- runTestOutput input (runRequest handler)
      ByteString.unpack (stderr result) `shouldContain` "test debug message"

  describe "handleRequest" $ do
    let request = Request {
        requestMethod = ""
      , requestArguments = []
      , requestContext = HashMap.empty
    }
    context "when message is unknown" $ do
      it "should throw 'NotImplemented'" $ do
        let request' = request {
          requestMethod = "unknown_cpi_method"
        }
        NotImplemented message <- runError () (runCpi HandleConfig (handleRequest request'))
        message `shouldBe` "Unknown method call 'unknown_cpi_method'"
    context "when message is 'create_stemcell'" $ do
      it "should run 'createStemcell'" $ do
        let request' = request {
            requestMethod = "create_stemcell"
          , requestArguments = [
              String "/path/to/stemcell"
            , Object HashMap.empty
          ]
        }
        (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
        (fromJust.responseResult) response `shouldBe` Id "stemcellId"
        output `shouldBe` [CreateStemcell
                            ("/path/to/stemcell")
                            (StemcellProperties $ Object $ HashMap.empty)
                           ]
    context "when message is 'create_vm'" $ do
      it "should run 'createVm'" $ do
        let request' = request {
            requestMethod = "create_vm"
          , requestArguments = [
              String "agent-id"
            , String "stemcell-id"
            , Object HashMap.empty
            , Object HashMap.empty
            , Array Vector.empty
            , Object HashMap.empty
          ]
        }
        (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
        (fromJust.responseResult) response `shouldBe` Id "vmId"
        output `shouldBe` [CreateVm
                            (AgentId "agent-id")
                            (StemcellId "stemcell-id")
                            (VmProperties $ Object $ HashMap.empty)
                            (Networks $ Object $ HashMap.empty)
                            ([])
                            (Environment $ HashMap.empty)
                           ]
    context "when message is 'has_vm'" $ do
      it "should run 'hasVm'" $ do
       let request' = request {
           requestMethod = "has_vm"
         , requestArguments = [
             String "vm-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Boolean True
       output `shouldBe` [HasVm
                           (VmId "vm-id")
                          ]
    context "when message is 'delete_vm'" $ do
      it "should run 'deleteVm'" $ do
       let request' = request {
           requestMethod = "delete_vm"
         , requestArguments = [
             String "vm-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Id ""
       output `shouldBe` [DeleteVm
                           (VmId "vm-id")
                          ]
    context "when message is 'create_disk'" $ do
      it "should run 'createDisk'" $ do
        let request' = request {
            requestMethod = "create_disk"
          , requestArguments = [
              Number 5000
            , (Object HashMap.empty)
            , String "vm-id"
          ]
        }
        (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
        (fromJust.responseResult) response `shouldBe` Id "diskId"
        output `shouldBe` [CreateDisk
                            (5000)
                            (DiskProperties $ Object $ HashMap.empty)
                            (VmId "vm-id")
                           ]
    context "when message is 'has_disk'" $ do
      it "should run 'hasDisk'" $ do
       let request' = request {
           requestMethod = "has_disk"
         , requestArguments = [
             String "disk-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Boolean True
       output `shouldBe` [HasDisk
                           (DiskId "disk-id")
                          ]
    context "when message is 'delete_disk'" $ do
      it "should run 'deleteDisk'" $ do
       let request' = request {
           requestMethod = "delete_disk"
         , requestArguments = [
             String "disk-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Id ""
       output `shouldBe` [DeleteDisk
                           (DiskId "disk-id")
                          ]
    context "when message is 'attach_disk'" $ do
      it "should run 'attachDisk'" $ do
       let request' = request {
           requestMethod = "attach_disk"
         , requestArguments = [
               String "vm-id"
             , String "disk-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Id ""
       output `shouldBe` [AttachDisk
                           (VmId "vm-id")
                           (DiskId "disk-id")
                          ]
    context "when message is 'detach_disk'" $ do
      it "should run 'detachDisk'" $ do
       let request' = request {
           requestMethod = "detach_disk"
         , requestArguments = [
               String "vm-id"
             , String "disk-id"
         ]
       }
       (response, output) <- runTest () (runCpi HandleConfig (handleRequest request'))
       (fromJust.responseResult) response `shouldBe` Id ""
       output `shouldBe` [DetachDisk
                           (VmId "vm-id")
                           (DiskId "disk-id")
                          ]

data HandleConfig = HandleConfig
type HandleOutput = [SingleOutput]
data SingleOutput = S Text deriving (Eq, Show)
data CpiCall = CreateStemcell FilePath StemcellProperties
             | CreateVm AgentId StemcellId VmProperties Networks DiskLocality Environment
             | HasVm VmId
             | DeleteVm VmId
             | CreateDisk Integer DiskProperties VmId
             | HasDisk DiskId
             | DeleteDisk DiskId
             | AttachDisk VmId DiskId
             | DetachDisk VmId DiskId
                deriving (Eq, Show)

instance MonadCpi HandleConfig (TestSystem () [CpiCall]) where
  parseConfig raw = pure HandleConfig
  createStemcell a b = do
    lift $ tell [CreateStemcell a b]
    pure $ StemcellId "stemcellId"
  createVm a b c d e f = do
    lift $ tell [CreateVm a b c d e f]
    pure $ VmId "vmId"
  hasVm a = do
    lift $ tell [HasVm a]
    pure True
  deleteVm a = do
    lift $ tell [DeleteVm a]
    pure ()
  createDisk a b c = do
    lift $ tell [CreateDisk a b c]
    pure $ DiskId "diskId"
  hasDisk a = do
    lift $ tell [HasDisk a]
    pure True
  deleteDisk a = do
    lift $ tell [DeleteDisk a]
    pure ()
  attachDisk a b = do
    lift $ tell [AttachDisk a b]
    pure ()
  detachDisk a b = do
    lift $ tell [DetachDisk a b]
    pure ()

instance FileSystem (TestSystem () [CpiCall])
instance System (TestSystem () [CpiCall])
