module CPI.Base.RequestSpec(spec) where

import           CPI.Base.Request
import           CPI.Base.TestSupport
import qualified Data.HashMap.Strict  as HashMap
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseRequest" $ do
    it "should parse a request with 'method', 'arguments' and 'context' fields" $ do
      result <- runTestResult mkTestInput (parseRequest "{\"method\":\"testMethod\",\"arguments\":[],\"context\":{}}")
      result `shouldBe` Request {
           requestMethod = "testMethod"
         , requestArguments = []
         , requestContext = HashMap.empty
       }
