module CPI.Base.SystemSpec(spec) where

import           CPI.Base.Errors
import           CPI.Base.System
import           CPI.Base.TestSupport
import           Test.Hspec

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "should load the content of the file given as the first commandline argument" $ do
      let input = mkTestInput {
              args = ["file"]
            , fileContent = "content"
          }
      result <- runTestResult input loadConfig
      result `shouldBe` "content"
    context "when there is no file specified via commandline argument" $ do
      it "should throw a `CloudError`" $ do
        let input = mkTestInput {
                args = []
              , fileContent = "content"
            }
        result <- runError input loadConfig
        result `shouldBe` CloudError "No config file location provided"
  describe "readRequest" $ do
    it "should read content from `stdin`" $ do
      let input = mkTestInput {
            stdinContent = "content"
          }
      result <- runTestResult input readRequest
      result `shouldBe` "content"
  describe "writeResponse" $ do
    it "should write to `stdout`" $ do
      result <- runTestOutput mkTestInput (writeResponse "content")
      result `shouldBe` mkTestOutput {stdout = "content"}
