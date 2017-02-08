module CPI.Base.Response(
    Response(..)
  , ResultType(..)
) where

import           Data.Aeson.Types
import           Data.Text        (Text)

data Response =  Response {
    responseResult :: ResultType
} deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response responseResult) =
        object ["result" .= responseResult]
    toEncoding (Response responseResult) =
        pairs ("result" .= responseResult)

data ResultType = Id Text | Boolean Bool deriving (Eq, Show)
instance ToJSON ResultType where
    toJSON (Id text)      = String text
    toJSON (Boolean text) = Bool text
