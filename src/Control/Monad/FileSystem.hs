module Control.Monad.FileSystem(
    MonadFileSystem(..)
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Text       (Text)
import qualified Data.Text       as Text

class (Monad m) => MonadFileSystem m where
  readFile :: Text -> m ByteString

instance MonadFileSystem IO where
  readFile = ByteString.readFile . Text.unpack
