{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.WaitSpec(spec) where

import Control.Monad.Wait
import Control.Monad.Time
import Control.Monad.State

import Data.Hourglass
import qualified Data.HashMap.Strict as HashMap

import Control.Monad.Stub.StubMonad
import Control.Monad.Stub.Time
import Control.Monad.TestSupport

import           Test.Hspec

import Data.Text (Text)

runStubT' :: TestInput -> [a] -> StubT TestInput [a] TestOutput IO (Maybe Text) -> IO (Maybe Text, [a], TestOutput)
runStubT' = runStubT

instance HasTime [Text] where
  asTime = const 0
  updateTime s _ = s

instance HasTimeline [Text] where
  asTimeline = const HashMap.empty
  updateTimeline s _ = s

spec :: Spec
spec = describe "MonadWait" $ do
  describe "wait" $ do
    it "should not continue before the specified amount of time passed" $ do
      void $ runEffects
        (do
          timeBefore <- currentTime
          wait $ Seconds 1
          timeAfter <- currentTime
          let timeElapsed = timeAfter `timeDiff` timeBefore
          liftIO $ timeElapsed >= 1 `shouldBe` True
        )
  describe "waitFor" $ do
    let getter :: (MonadState [Text] m) => m (Maybe Text)
        getter = do
          x:xs <- get
          put xs
          pure $ Just x

    context "when configured to retry 9 times with an interval of 1s" $ do
      let waitConfig = WaitConfig {
          retries = Retry 9
        , interval = Seconds 1
      }

      it "should wait 10x1s for resource to fulfill the predicate" $ do
        (_, _, output) <- runStubT'
                emptyTestInput
                (replicate 10 "Not Done" ++ ["Done"])
                (do r <- waitFor waitConfig getter (== (Just "Done"))
                    lift $ r `shouldBe` Just "Done"
                    pure r)
        length (waitCount output) `shouldBe` 10
        all (== 1) (waitCount output) `shouldBe` True

      it "should throw after 10x1s if the predicate is never fulfilled" $ do
        void $ do
          r@(_, _, output) <- runStubT'
                emptyTestInput
                (repeat "Never Done")
                (waitFor waitConfig getter (== (Just "Done")))
          length (waitCount output) `shouldBe` 9
          all (== 1) (waitCount output) `shouldBe` True
          pure r
        `shouldThrow` timeout

    context "when configured to retry unlimited" $ do
      let waitConfig = WaitConfig {
          retries = Unlimited
        , interval = Seconds 1
      }
      it "should wait for resource to fulfill the predicate" $ do
        (_, _, output) <- runStubT'
                emptyTestInput
                (replicate 100 "Not Done" ++ ["Done"])
                (do r <- waitFor waitConfig getter (== (Just "Done"))
                    lift $ r `shouldBe` Just "Done"
                    pure r)
        length (waitCount output) `shouldBe` 100
        all (== 1) (waitCount output) `shouldBe` True

timeout :: Selector Timeout
timeout Timeout = True
