{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module CPI.Base(
    module Base
  , Cpi(..)
  , MonadCpi(..)
  , runRequest
) where

import           Prelude                      hiding (readFile)

import           CPI.Base.Errors              as Base
import           CPI.Base.Request             as Base
import           CPI.Base.Response            as Base
import           CPI.Base.System              as Base

import           Control.Monad.Reader

import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)

import           Control.Exception.Safe
import           Data.Aeson


import           Control.Monad.Log
import           Text.PrettyPrint.Leijen.Text hiding ((<$>))

runRequest :: (MonadCpi c m) => (Request -> Cpi c m Response) -> m ()
runRequest handleRequest = do
  config <- loadConfig
            >>= parseConfig
  request <- readRequest
            >>= parseRequest
  response <-
      runCpi (handleRequest request)
        `runReaderT` config
  writeResponse $ toStrict $ encode response

newtype (Monad m, MonadCpi c m, System m) => Cpi c m a = Cpi {
  runCpi :: ReaderT c m a
} deriving (Functor, Applicative, Monad, MonadReader c, MonadThrow, MonadTrans)

instance (System m) => System (Cpi c m) where
  arguments = lift arguments
  readFile = lift.readFile
  readStdin = lift readStdin
  writeStdout = lift.writeStdout
  writeStderr = lift.writeStderr

class (MonadThrow m, MonadLog (WithSeverity Doc) m, System m) => MonadCpi c m | c -> m where
  parseConfig :: ByteString -> m c
