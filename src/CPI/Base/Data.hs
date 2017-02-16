{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Base.Data(
    AgentId(..)
  , Environment(..)
) where

import           Control.Lens
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)

newtype AgentId = AgentId Text
    deriving (Eq, Show, FromJSON, ToJSON)

newtype Environment = Environment (HashMap Text Text)
    deriving (Eq, Show, FromJSON, ToJSON)

makeWrapped ''AgentId
makeWrapped ''Environment
