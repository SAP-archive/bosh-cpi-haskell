{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module CPI.Base(
    module Base
  , CpiRunner(..)
  , MonadCpi(..)
  , CpiConfiguration(..)
  , runRequest
  , handleRequest
) where

import           Prelude                      hiding (readFile)

import           CPI.Base.AgentConfig         as Base
import           CPI.Base.Data                as Base
import           CPI.Base.Errors              as Base
import           CPI.Base.Request             as Base
import           CPI.Base.Response            as Base
import           CPI.Base.System              as Base

import           Control.Monad.Reader

import           Control.Monad.Arguments
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.Wait

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as ByteString
import           Data.ByteString.Lazy         (toStrict)
import           Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)
import           Data.Text.Lazy               (fromStrict)

import           Control.Exception.Safe
import           Data.Aeson
import           Data.Semigroup

import           Control.Monad.Log
import           Text.PrettyPrint.Leijen.Text (Doc, text)
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))


runRequest :: ( Monad m
                , MonadReader c m
                , Monad m'
                , MonadCatch m'
                , MonadArguments m'
                , MonadFileSystem m'
                , CpiConfiguration c m'
                , CpiRunner c m m' Response) => (Request -> m Response) -> m' ()
runRequest handleRequest = do
  let execute = do
        config <- loadConfig >>= parseConfig
        request <- readRequest >>= parseRequest
        config `runCpi` handleRequest request
  response <- execute `catch` (pure.createFailure)
  writeResponse $ toStrict $ encode response

handleRequest :: (MonadCpi c m) => (Request -> m Response)
handleRequest request@Request{
    requestMethod = method
  , requestArguments = arguments
  , requestContext = context
} = case (method, arguments) of
      ("create_stemcell", [filePath, cloudProperties]) -> do
        StemcellId stemcellId <- join $ createStemcell
                          <$> parseArgument filePath
                          <*> parseArgument cloudProperties
        return $ createSuccess $ Id stemcellId
      ("delete_stemcell", [stemcellId]) -> do
        join $ deleteStemcell
            <$> parseArgument stemcellId
        return $ createSuccess (Id "")
      ("create_vm", [agentId, stemcellId, vmProperties, networks, diskLocality, environment]) -> do
        VmId vmCid <- join $ createVm
                     <$> parseArgument agentId
                     <*> parseArgument stemcellId
                     <*> parseArgument vmProperties
                     <*> parseArgument networks
                     <*> parseArgument diskLocality
                     <*> parseArgument environment
        pure $ createSuccess $ Id vmCid
      ("has_vm", [vmId]) -> do
        hasVm <- join $ hasVm
                     <$> parseArgument vmId
        return $ createSuccess (Boolean hasVm)
      ("delete_vm", [vmId]) -> do
        join $ deleteVm
            <$> parseArgument vmId
        return $ createSuccess (Id "")
      ("create_disk", [size, diskProperties, vmId]) -> do
        DiskId diskCid <- join $ createDisk
            <$> parseArgument size
            <*> parseArgument diskProperties
            <*> parseArgument vmId
        return $ createSuccess (Id diskCid)
      ("has_disk", [diskId]) -> do
        hasDisk <- join $ hasDisk
                     <$> parseArgument diskId
        return $ createSuccess (Boolean hasDisk)
      ("delete_disk", [diskId]) -> do
        join $ deleteDisk
            <$> parseArgument diskId
        return $ createSuccess (Id "")
      ("attach_disk", [vmId, diskId]) -> do
        join $ attachDisk
            <$> parseArgument vmId
            <*> parseArgument diskId
        return $ createSuccess (Id "")
      ("detach_disk", [vmId, diskId]) -> do
        join $ detachDisk
            <$> parseArgument vmId
            <*> parseArgument diskId
        return $ createSuccess (Id "")
      _ -> throwM $ NotImplemented ("Unknown method call '" <> method <> "'")

class ( MonadThrow m
      , MonadLog (WithSeverity Text) m
      , MonadConsole m)
      => CpiConfiguration c m | c -> m where
  parseConfig :: ByteString -> m c

class ( Monad m
      , MonadReader c m
      , Monad m'
      , MonadArguments m'
      , MonadFileSystem m'
      , CpiConfiguration c m')
      => CpiRunner c m m' a where
  runCpi :: c -> m a -> m' a

class ( MonadReader c m
      , MonadThrow m
      , MonadLog (WithSeverity Text) m
      , MonadConsole m)
      => MonadCpi c m | c -> m where
  createStemcell :: FilePath -> StemcellProperties -> m StemcellId
  deleteStemcell :: StemcellId -> m ()
  createVm :: AgentId -> StemcellId -> VmProperties -> Networks
              -> DiskLocality -> Environment -> m VmId
  hasVm :: VmId -> m Bool
  deleteVm :: VmId -> m ()
  createDisk :: Integer -> DiskProperties -> VmId -> m DiskId
  hasDisk :: DiskId -> m Bool
  deleteDisk :: DiskId -> m ()
  attachDisk :: VmId -> DiskId -> m ()
  detachDisk :: VmId -> DiskId -> m ()
