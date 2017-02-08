module CPI.Base.Errors(
    CloudError(..)
) where

import           Control.Exception      (Exception)
import           Control.Exception.Safe
import           Data.Text              (Text)

data CloudError = CloudError Text
    deriving (Typeable, Show, Eq)

instance Exception CloudError
