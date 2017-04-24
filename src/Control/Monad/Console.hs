module Control.Monad.Console(
   MonadConsole(..)
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import           System.IO       (stderr)

class (Monad m) => MonadConsole m where
  readStdin   :: m ByteString
  writeStdout :: ByteString -> m ()
  writeStderr :: ByteString -> m ()

instance MonadConsole IO where
  readStdin = ByteString.getContents
  writeStdout = ByteString.putStr
  writeStderr = ByteString.hPutStr stderr
