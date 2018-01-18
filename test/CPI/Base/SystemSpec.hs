{-# LANGUAGE ScopedTypeVariables #-}

module CPI.Base.SystemSpec(spec) where

import           CPI.Base.Errors
import           CPI.Base.System
import           CPI.Base.TestSupport
import           Test.Hspec

import           Control.Exception.Safe

import           Control.Effect.Stub

import           Data.ByteString        (ByteString)
import qualified Data.HashMap.Strict    as HashMap

type Stub a = StubT TestInput TestOutput TestState IO a

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "should load the content of the file given as the first commandline argument" $ do
      let input = mkTestInput {
              args = ["file"]
          }
          state = mkTestState {
            fileSystem = HashMap.singleton "file" "content"
          }
      (result, _, ()) <- runStubT input state loadConfig
      result `shouldBe` "content"
    context "when there is no file specified via commandline argument" $ do
      it "should throw a `CloudError`" $ do
        result <- try (runStubT mkTestInput mkTestState (loadConfig :: Stub ByteString))
        case result of
          Right (result, _, _::TestOutput) -> error $ "Unexpected result of `runTestResult`: " ++ show result
          Left err     -> err `shouldBe` CloudError "No config file location provided"


  describe "readRequest" $ do
    it "should read content from `stdin`" $ do
      let input = mkTestInput {
            stdinContent = "content"
          }
      (result, _, _::TestOutput) <- runStubT input mkTestState (readRequest :: Stub ByteString)
      result `shouldBe` "content"
  describe "writeResponse" $ do
    it "should write to `stdout`" $ do
      (_, _, output) <- runStubT mkTestInput mkTestState (writeResponse "content")
      output `shouldBe` mkTestOutput {stdout = "content"}
