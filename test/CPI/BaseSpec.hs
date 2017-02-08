{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as ByteString hiding (unpack)
import qualified Data.ByteString.Char8      as ByteString
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

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
          handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            config <- ask
            if config /= TestConfig "content"
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected configuration " ++ show config
              else pure Response {
                responseResult = Id "id"
              }
      result <- runTestResult input' (runRequest handler)
      result `shouldBe` ()
    it "should read and parse the request" $ do
      let handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            if request /= Request {
                              requestMethod = "testMethod"
                            , requestArguments = []
                            , requestContext = HashMap.empty
                          }
              -- TODO expectation does not belong here. How can be bring it to the outside?
              then throw $ CloudError $ "Unexpected request " ++ show request
              else pure Response {
                responseResult = Id "id"
              }
      result <- runTestResult input (runRequest handler)
      result `shouldBe` ()
    context "when the result type is a string" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig TestSystem Response
            handler request =
              pure Response {
                responseResult = Id "id"
              }
        result <- runTestOutput input (runRequest handler)
        stdout result `shouldBe` "{\"result\":\"id\"}"
    context "when the result type is a boolean" $ do
      it "should write the response" $ do
        let handler :: Request -> Cpi TestConfig TestSystem Response
            handler request =
              pure Response {
                responseResult = Boolean True
              }
        result <- runTestOutput input (runRequest handler)
        stdout result `shouldBe` "{\"result\":true}"
    it "provides logging facilities" $ do
      let handler :: Request -> Cpi TestConfig TestSystem Response
          handler request = do
            logDebug "test debug message"
            pure Response {
              responseResult = Id "id"
            }
      result <- runTestOutput input (runRequest handler)
      ByteString.unpack (stderr result) `shouldContain` "test debug message"
