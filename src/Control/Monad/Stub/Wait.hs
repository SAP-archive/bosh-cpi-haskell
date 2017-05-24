{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Stub.Wait(
    HasWaitCount(..)
) where

import           Control.Exception.Safe
import           Control.Monad.State
import           Control.Monad.Time
import           Control.Monad.Wait
import           Control.Monad.Writer         hiding ((<>))

import           Control.Monad.Stub.StubMonad
import           Control.Monad.Stub.Time

import           Data.HashMap.Strict          as HashMap
import           Data.HashMap.Strict          (HashMap)
import           Data.Hourglass
import           Data.Maybe

class (Monoid a) => HasWaitCount a where
  asWaitCount :: TimeInterval i => i -> a
  asWaitCount = mempty

instance (Monad m, MonadThrow m, Monoid w, HasWaitCount w, HasTime s, HasTimeline s) => MonadWait (StubT r s w m) where
  wait n = do
    tick n
    tell $ asWaitCount n
