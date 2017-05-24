{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Control.Monad.Stub.Time(
    HasTime(..)
  , HasTimeline(..)
  , MonadFakeTime(..)
) where

import           Data.Hourglass
import           Time.System
import           Time.Types

import           Control.Monad.Time

import           Control.Monad.State
import           Control.Monad.Stub.StubMonad

import           Data.Hashable
import           Data.HashMap.Strict          as HashMap
import           Data.HashMap.Strict          (HashMap)
import           Data.Maybe

class HasTime s where
  asTime :: s -> Elapsed
  updateTime :: s -> Elapsed -> s
  withTime :: (Elapsed -> Elapsed) -> s -> s
  withTime f s = s `updateTime` f (asTime s)

instance (Monad m, Monoid w, HasTime s) => MonadTime (StubT r s w m) where
  currentTime = gets asTime

class HasTimeline s where
  asTimeline :: s -> HashMap Elapsed [s -> s]
  updateTimeline :: s -> HashMap Elapsed [s -> s] -> s
  withTimeline :: (HashMap Elapsed [s -> s] -> HashMap Elapsed [s -> s]) -> s -> s
  withTimeline f s = s `updateTimeline` f (asTimeline s)

instance Hashable Elapsed where
  hashWithSalt s (Elapsed (Seconds n)) = s + hash n

class (MonadState s m) => MonadFakeTime s m where
  tick :: TimeInterval i => i -> m ()

instance (Monad m, HasTime s, HasTimeline s, Monoid w) => MonadFakeTime s (StubT r s w m) where
  tick n = do
    current <- currentTime
    timeline <- gets asTimeline
    let next = current `timeAdd` n
        events = concat
               $ HashMap.elems
               $ HashMap.filterWithKey
                 (\k _ -> current < k && k <= next)
                 timeline
    mapM_ modify events
    modify $ withTime $ const next
