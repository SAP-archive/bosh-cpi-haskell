{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Base.Data(
    AgentId(..)
) where

import           Control.Lens
import           Data.Aeson
import           Data.Text    (Text)

newtype AgentId = AgentId Text
    deriving (Eq, Show, FromJSON, ToJSON)

makeWrapped ''AgentId
