{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module CPI.Base(
    module Base
  , Cpi(..)
  , runCpi
  , MonadCpi(..)
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

import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)
import           Data.HashMap.Strict          as HashMap
import           Data.Text                    (Text)

import           Control.Exception.Safe
import           Data.Aeson
import           Data.Semigroup

import           Control.Monad.Log
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

runRequest :: (MonadCatch m, MonadCpi c m) => (Request -> Cpi c m Response) -> m ()
runRequest handleRequest = do
  response <- do
        config <- loadConfig
                  >>= parseConfig
        request <- readRequest
                  >>= parseRequest
        runCpi config (handleRequest request)
   `catch` (pure.createFailure)
  writeResponse $ toStrict $ encode response

handleRequest :: (MonadCpi c m) => (Request -> Cpi c m Response)
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


newtype (Monad m, MonadCpi c m, System m) => Cpi c m a = Cpi {
  unCpi :: ReaderT c m a
} deriving (Functor, Applicative, Monad, MonadReader c, MonadThrow, MonadCatch, MonadTrans)

runCpi :: MonadCpi c m => c -> Cpi c m a -> m a
runCpi config cpi = unCpi cpi `runReaderT` config

instance (FileSystem m) => FileSystem (Cpi c m) where
  readFile = lift.readFile

instance (System m) => System (Cpi c m) where
  arguments = lift arguments
  readStdin = lift readStdin
  writeStdout = lift.writeStdout
  writeStderr = lift.writeStderr

class (MonadThrow m, MonadLog (WithSeverity Text) m, System m) => MonadCpi c m | c -> m where
  parseConfig :: ByteString -> m c
  createStemcell :: FilePath -> StemcellProperties -> Cpi c m StemcellId
  deleteStemcell :: StemcellId -> Cpi c m ()
  createVm :: AgentId -> StemcellId -> VmProperties -> Networks
              -> DiskLocality -> Environment -> Cpi c m VmId
  hasVm :: VmId -> Cpi c m Bool
  deleteVm :: VmId -> Cpi c m ()
  createDisk :: Integer -> DiskProperties -> VmId -> Cpi c m DiskId
  hasDisk :: DiskId -> Cpi c m Bool
  deleteDisk :: DiskId -> Cpi c m ()
  attachDisk :: VmId -> DiskId -> Cpi c m ()
  detachDisk :: VmId -> DiskId -> Cpi c m ()
