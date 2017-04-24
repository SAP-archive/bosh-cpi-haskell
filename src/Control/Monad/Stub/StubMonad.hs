{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Stub.StubMonad(
    StubT(..)
  , runStubT
  , runStubTResult
  , runStubTOutput
  , runStubTState
) where

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

newtype StubT r s w m a = StubT {
 unStubT :: ReaderT r (StateT s (WriterT w m)) a
} deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadWriter w, MonadThrow, MonadCatch)

instance (Monoid w) => MonadTrans (StubT r s w) where
  lift = StubT . lift . lift . lift

runStubT :: (Monad m) => r -> s -> StubT r s w m a -> m (a, s, w)
runStubT r s stubs = do
  ((a, s), w) <- runWriterT (runStateT (runReaderT (unStubT stubs) r) s)
  pure (a, s, w)

runStubTResult :: (Monad m) => r -> s -> StubT r s w m a -> m a
runStubTResult r s stubs = do
  (a, _, _) <- runStubT r s stubs
  pure a

runStubTState :: (Monad m) => r -> s -> StubT r s w m a -> m s
runStubTState r s stubs = do
  (_, s, _) <- runStubT r s stubs
  pure s

runStubTOutput :: (Monad m) => r -> s -> StubT r s w m a -> m w
runStubTOutput r s stubs = do
  (_, _, w) <- runStubT r s stubs
  pure w
