{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CPI.Base.System(
    System(..)
  , loadConfig
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

import           Control.Exception.Safe
import           Control.Monad.Log
import           System.IO                    (stderr)
import           Text.PrettyPrint.Leijen.Text (Doc, text)

class MonadThrow m => System m where
  arguments :: m [Text]
  readFile :: Text -> m ByteString
  readStdin :: m ByteString
  writeStdout :: ByteString -> m ()
  writeStderr :: ByteString -> m ()

instance System IO where
  arguments = getArgs >>= pure . fmap Text.pack
  readFile = ByteString.readFile . Text.unpack
  readStdin = ByteString.getContents
  writeStdout = ByteString.putStr
  writeStderr = ByteString.hPutStr stderr

-- TODO can we get rid of this orphan?
instance (Monad m, System m) => MonadLog (WithSeverity Text) m where
  logMessageFree f = writeStderr $ f (ByteString.pack.show.renderWithSeverity (text.fromStrict))

loadConfig :: (System m) => m ByteString
loadConfig = do
  args <- arguments
  if not (null args)
    then readFile $ head args
    else throw $ CloudError "No config file location provided"

readRequest :: (System m) => m ByteString
readRequest = readStdin

writeResponse :: (System m) => ByteString -> m ()
writeResponse = writeStdout
