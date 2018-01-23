{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPI.Base.RequestSpec(spec) where

import           CPI.Base.Request
import           CPI.Base.TestSupport
import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.QQ

import           Data.ByteString      (ByteString)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Semigroup
import qualified Data.Vector          as Vector

spec :: Spec
spec = do
  describe "parseRequest" $ do
    it "should parse a request with 'method', 'arguments' and 'context' fields" $ do
      result <- parseRequest "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}"
      result `shouldBe` Request {
           requestMethod = "testMethod"
         , requestArguments = []
         , requestContext = HashMap.empty
       }
    context "given an invalid request" $ do
      it "throws a CloudError" $ do
        parseRequest "" `shouldThrow` cloudErrorWithMessage "Could not parse request: 'Error in $: not enough input'"
  describe "parseArgument" $ do
    context "given a json string" $ do
      it "provides an aeson 'String'" $ do
        arg <- parseArgument "some string"
        arg `shouldBe` String "some string"
    context "given a json object" $ do
      it "provides an aeson 'Object'" $ do
        arg <- parseArgument [aesonQQ|{"key": "value"}|]
        arg `shouldBe` (Object $ HashMap.singleton "key" "value")
    context "given a json array" $ do
      it "provides an aeson 'Array'" $ do
        arg <- parseArgument [aesonQQ|["value"]|]
        arg `shouldBe` (Array $ Vector.singleton "value")
    context "given a json array but expecting an aeson 'Object'" $ do
      it "throws a cloud error" $ do
        (parseArgument :: Value -> IO Object) [aesonQQ|["value"]|]
          `shouldThrow`
            cloudErrorWithMessage ("Could not parse value '[\"value\"]': 'expected HashMap ~Text v, encountered Array'")
