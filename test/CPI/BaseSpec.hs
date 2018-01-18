{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module CPI.BaseSpec(spec) where

import           CPI.Base
import           CPI.Base.TestSupport
import           Test.Hspec

import           Control.Effect.Class.Console
import           Control.Effect.Stub

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.RWS.Lazy (RWST (..), runRWST)
import           Control.Monad.Writer
import           Data.Either
import           Data.Maybe

import           Data.Aeson
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString hiding (unpack)
import qualified Data.ByteString.Char8        as ByteString
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Vector                  as Vector hiding (force, (++))

import           Data.Aeson.QQ

import           Control.Monad.Log


import           Control.Exception.Safe
import           Control.Lens

spec :: Spec
spec = do
  runRequestTests
  runHandleRequestTests

runRequestTests =
  describe "runRequest" $ do
    let input = mkTestInput {
            args = ["config"]
          , stdinContent = "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}"}
        state = mkTestState
    it "provides parsed configuration via reader" $ do
      let state' = state {
            fileSystem = HashMap.singleton "config" "content"
          }
          handler request = do
            config <- ask
            lift $ config `shouldBe` TestConfig "content"
            pure $ createSuccess $ Id "id"
      (_, _, output) <- runRunRequest input state' handler
      (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|{"result":"id", error:null, "log":""}|]
    it "should read and parse the request" $ do
      let handler request = do
            lift $ request
                    `shouldBe` Request {
                                    requestMethod = "testMethod"
                                  , requestArguments = []
                                  , requestContext = HashMap.empty
                                }
            pure $ createSuccess $ Id "id"
      (result, _, _) <- runRunRequest input state handler
      result `shouldBe` ()
    context "when the result type is a string" $ do
      it "should write the response" $ do
        let handler request =
              pure $ createSuccess $ Id "id"
        (_, _, output) <- runRunRequest input state handler
        (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|{"result":"id", error:null, "log":""}|]
    context "when the result type is a boolean" $ do
      it "should write the response" $ do
        let handler request =
              pure $ createSuccess $ Boolean True
        (_, _, output) <- runRunRequest input state handler
        (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|{"result":true, error:null, "log":""}|]
    context "when an exception is thrown" $ do
      it "should write an error response" $ do
        let handler request = throwM $ DummyException "BOOM!!!"
        (_, _, output) <- runRunRequest input state handler
        (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|
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
        let handler request = throwM $ CloudError "BOOM!!!"
        (_, _, output) <- runRunRequest input state handler
        (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|
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
        let handler request = throwM $ NotImplemented "BOOM!!!"
        (_, _, output) <- runRunRequest input state handler
        (eitherDecode'.fromStrict.stdout) output `shouldBe` Right [aesonQQ|
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
      let handler request = do
            logDebug "test debug message"
            pure $ createSuccess $ Id "id"
      (_, _, output) <- runRunRequest input state handler
      ByteString.unpack (stderr output) `shouldContain` "test debug message"

runRunRequest :: TestInput -> TestState -> (Request -> StubT TestConfig TestOutput TestState IO Response) -> IO ((), TestState, TestOutput)
runRunRequest input state handler =
  runStubT input state $ runRequest handler

instance HasStdin TestConfig

instance Cpi TestConfig (StubT TestConfig TestOutput TestState IO) where
  type VmProperties TestConfig = ()

instance CpiConfiguration TestConfig (StubT TestInput TestOutput TestState IO) where
  parseConfig raw = pure $ TestConfig raw

instance CpiRunner TestConfig (StubT TestConfig TestOutput TestState IO) (StubT TestInput TestOutput TestState IO) a where
  runCpi config m =
    StubT $ RWST $ \r s -> runStubT config s m

runHandleRequestTests =
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
        result <- try( runHandleRequest request' )
        case result of
          Right _ -> error $ "Unexpected result of `runTestResult`: " ++ show result
          Left err     -> err `shouldBe` NotImplemented "Unknown method call 'unknown_cpi_method'"
    context "when message is 'create_stemcell'" $ do
      it "should run 'createStemcell'" $ do
        let request' = request {
            requestMethod = "create_stemcell"
          , requestArguments = [
              String "/path/to/stemcell"
            , Object HashMap.empty
          ]
        }
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id "stemcellId"
        calls `shouldBe` [CreateStemcell
                            ("/path/to/stemcell")
                            (StemcellProperties $ Object $ HashMap.empty)
                            ]
    context "when message is 'delete_stemcell'" $ do
      it "should run 'deleteStemcell'" $ do
        let request' = request {
            requestMethod = "delete_stemcell"
          , requestArguments = [
              String "stemcell-id"
          ]
        }
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id ""
        calls `shouldBe` [DeleteStemcell
                            (StemcellId "stemcell-id")
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id "vmId"
        calls `shouldBe` [CreateVm
                            (AgentId "agent-id")
                            (StemcellId "stemcell-id")
                            (Object $ HashMap.empty)
                            (Wrapped $ HashMap.empty)
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Boolean True
        calls `shouldBe` [HasVm
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id ""
        calls `shouldBe` [DeleteVm
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id "diskId"
        calls `shouldBe` [CreateDisk
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Boolean True
        calls `shouldBe` [HasDisk
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id ""
        calls `shouldBe` [DeleteDisk
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id ""
        calls `shouldBe` [AttachDisk
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
        (response, calls) <- runHandleRequest request'
        (fromJust.responseResult) response `shouldBe` Id ""
        calls `shouldBe` [DetachDisk
                            (VmId "vm-id")
                            (DiskId "disk-id")
                          ]


-- (response, state, output) <- runStubT () () (runCpi () (handleRequest request'))
runHandleRequest :: Request -> IO (Response, [CpiCall])
runHandleRequest request = do
  let handledRequest :: StubT () [CpiCall] () IO Response = handleRequest request
      unStubbed :: IO (Response, (), [CpiCall]) = runStubT () () $ handledRequest
  (response, (), calls) <- unStubbed
  pure (response, calls)

data CpiCall = CreateStemcell FilePath StemcellProperties
              | DeleteStemcell StemcellId
              | CreateVm AgentId StemcellId Value Networks DiskLocality Environment
              | HasVm VmId
              | DeleteVm VmId
              | CreateDisk Integer DiskProperties VmId
              | HasDisk DiskId
              | DeleteDisk DiskId
              | AttachDisk VmId DiskId
              | DetachDisk VmId DiskId
                deriving (Eq, Show)

instance HasStdin () where
  asStdin = const ""

instance HasArguments () where
  asArguments = const []

instance HasStdout [CpiCall] where
  asStdout _ = []

instance HasStderr [CpiCall] where
  asStderr _ = []

instance Cpi () (StubT () [CpiCall] () IO) where
  type VmProperties () = Value
  createStemcell a b = do
    tell [CreateStemcell a b]
    pure $ StemcellId "stemcellId"
  deleteStemcell a = do
    tell [DeleteStemcell a]
    pure ()
  createVm a b c d e f = do
    tell [CreateVm a b c d e f]
    pure $ VmId "vmId"
  hasVm a = do
    tell [HasVm a]
    pure True
  deleteVm a = do
    tell [DeleteVm a]
    pure ()
  createDisk a b c = do
    tell [CreateDisk a b c]
    pure $ DiskId "diskId"
  hasDisk a = do
    tell [HasDisk a]
    pure True
  deleteDisk a = do
    tell [DeleteDisk a]
    pure ()
  attachDisk a b = do
    tell [AttachDisk a b]
    pure ()
  detachDisk a b = do
    tell [DetachDisk a b]
    pure ()

