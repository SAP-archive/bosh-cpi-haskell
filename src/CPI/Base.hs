module CPI.Base(
    System(..)
  , CloudError(..)
  , loadConfig
  , readRequest
  , writeResponse
) where

import           Prelude                hiding (readFile)

import           Control.Applicative

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Data.Typeable

import           System.Environment     (getArgs)

import           Control.Exception      (Exception)
import           Control.Exception.Safe

data CloudError = CloudError String
    deriving (Typeable, Show, Eq)

instance Exception CloudError

class MonadThrow m => System m where
  arguments :: m [Text]
  readFile :: Text -> m ByteString
  readStdin :: m ByteString
  writeStdout :: ByteString -> m ()

instance System IO where
  arguments = getArgs >>= pure . fmap Text.pack
  readFile = ByteString.readFile . Text.unpack
  readStdin = ByteString.getContents
  writeStdout = ByteString.putStr

loadConfig :: (Monad m, System m) => m ByteString
loadConfig = do
  args <- arguments
  if not (null args)
    then readFile $ head args
    else throw $ CloudError "No config file location provided"

readRequest :: (Monad m, System m) => m ByteString
readRequest = readStdin

writeResponse :: (Monad m, System m) => ByteString -> m ()
writeResponse = writeStdout
