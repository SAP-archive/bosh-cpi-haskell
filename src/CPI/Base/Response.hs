module CPI.Base.Response(
    Response(..)
  , ResultType(..)
  , createSuccess
  , createFailure
) where

import           CPI.Base.Errors

import           Data.Aeson.Types
import           Data.Semigroup
import           Data.Text        (Text)

data Response =  Response {
    responseResult :: Maybe ResultType
  , responseError  :: Maybe CpiError
} deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response responseResult responseError) =
        object ["result" .= responseResult, "error" .= responseError]
    toEncoding (Response responseResult responseError) =
        pairs ("result" .= responseResult <> "error" .= responseError)

data ResultType = Id Text | Boolean Bool deriving (Eq, Show)

instance ToJSON ResultType where
    toJSON (Id text)      = String text
    toJSON (Boolean text) = Bool text

createSuccess :: ResultType -> Response
createSuccess result = Response {
      responseResult = Just result
    , responseError = Nothing
  }

createFailure :: CloudError -> Response
createFailure (CloudError message) = Response {
      responseResult = Nothing
    , responseError = Just CpiError {
          errorType = "Bosh::Clouds::CloudError"
        , errorMessage = message
        , okToRetry = False
  }
  }

data CpiError = CpiError {
    errorType    :: Text
  , errorMessage :: Text
  , okToRetry    :: Bool
} deriving (Eq, Show)

instance ToJSON CpiError where
    toJSON (CpiError errorType errorMessage okToRetry) =
        object ["type" .= errorType, "message" .= errorMessage, "ok_to_retry" .= okToRetry]
    toEncoding (CpiError errorType errorMessage okToRetry) =
        pairs ("type" .= errorType <> "message" .= errorMessage <> "ok_to_retry" .= okToRetry)
