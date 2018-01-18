{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CPI.Base.System(
    loadConfig
  , readRequest
  , writeResponse
) where
import           Prelude                      hiding (readFile)

import           CPI.Base.Errors

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString hiding (pack)
import qualified Data.ByteString.Char8        as ByteString (pack)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (fromStrict)

import           System.Environment           (getArgs)

import           Control.Effect.Class.Arguments
import           Control.Effect.Class.Console
import           Control.Effect.Class.FileSystem

import           Control.Exception.Safe
import           Control.Monad.Log
import           System.IO                    (stderr)
import           Text.PrettyPrint.Leijen.Text (Doc, text)

-- TODO can we get rid of this orphan?
instance (Monad m, Console m) => MonadLog (WithSeverity Text) m where
  logMessageFree f = writeStderr $ f (ByteString.pack.show.renderWithSeverity (text.fromStrict))

loadConfig :: (MonadThrow m, FileSystem m, Arguments m) => m ByteString
loadConfig = do
  args <- arguments
  if not (Prelude.null args)
    then readFile $ head args
    else throw $ CloudError "No config file location provided"

readRequest :: (Console m) => m ByteString
readRequest = readStdin

writeResponse :: (Console m) => ByteString -> m ()
writeResponse = writeStdout
