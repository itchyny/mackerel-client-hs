{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Dashboard where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data DashboardId = DashboardId String
            deriving (Eq, Show)

instance FromJSON DashboardId where
  parseJSON (Aeson.String dashboardId') = return $ DashboardId $ Text.unpack dashboardId'
  parseJSON o = typeMismatch "DashboardId" o

instance ToJSON DashboardId where
  toJSON (DashboardId dashboardId') = toJSON dashboardId'

data DashboardCreate
  = DashboardCreate {
    dashboardCreateTitle :: String,
    dashboardCreateBodyMarkdown :: String,
    dashboardCreateUrlPath :: String
  } deriving (Eq, Show)

$(deriveJSON options { fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 15 } ''DashboardCreate)

data Dashboard
  = Dashboard {
    dashboardId :: DashboardId,
    dashboardTitle :: String,
    dashboardBodyMarkdown :: String,
    dashboardUrlPath :: String
  } deriving (Eq, Show)

$(deriveJSON options ''Dashboard)
