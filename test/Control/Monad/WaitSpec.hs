{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.WaitSpec(spec) where

import           Control.Monad.Wait
import Control.Monad.State

import Control.Monad.Stub.StubMonad
import Control.Monad.TestSupport

import           Test.Hspec

import Data.Text (Text)

runStubT' :: TestInput -> [a] -> StubT TestInput [a] TestOutput IO Text -> IO (Text, [a], TestOutput)
runStubT' = runStubT

spec :: Spec
spec = describe "waitFor" $ do
  let getter :: (MonadState [Text] m) => m (Maybe Text)
      getter = do
        x:xs <- get
        put xs
        pure $ Just x

  context "when configured to retry 9 times with an interval of 1000" $ do
    let waitConfig = WaitConfig {
        retries = Retry 9
      , interval = 1000
    }

    it "should wait 10x1000 for resource to fulfill the predicate" $ do
      (_, _, output) <- runStubT'
              emptyTestInput
              (replicate 10 "Not Done" ++ ["Done"])
              (do r <- waitFor waitConfig getter (== "Done")
                  lift $ r `shouldBe` "Done"
                  pure r)
      length (waitCount output) `shouldBe` 10
      all (== 1000) (waitCount output) `shouldBe` True

    it "should throw after 10x1000 if the predicate is never fulfilled" $ do
      void $ do
        r@(_, _, output) <- runStubT'
              emptyTestInput
              (repeat "Never Done")
              (waitFor waitConfig getter (== "Done"))
        length (waitCount output) `shouldBe` 9
        all (== 1000) (waitCount output) `shouldBe` True
        pure r
      `shouldThrow` timeout

  context "when configured to retry unlimited" $ do
    let waitConfig = WaitConfig {
        retries = Unlimited
      , interval = 1000
    }
    it "should wait for resource to fulfill the predicate" $ do
      (_, _, output) <- runStubT'
              emptyTestInput
              (replicate 100 "Not Done" ++ ["Done"])
              (do r <- waitFor waitConfig getter (== "Done")
                  lift $ r `shouldBe` "Done"
                  pure r)
      length (waitCount output) `shouldBe` 100
      all (== 1000) (waitCount output) `shouldBe` True


timeout :: Selector Timeout
timeout Timeout = True
