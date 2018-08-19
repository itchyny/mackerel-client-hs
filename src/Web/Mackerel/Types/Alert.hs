{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Alert where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, constructorTagModifier)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toUpper)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Host (HostId)
import Web.Mackerel.Types.Monitor (MonitorId, MonitorType)

data AlertId = AlertId String
             deriving (Eq, Show)

instance FromJSON AlertId where
  parseJSON (Aeson.String alertId') = return $ AlertId $ Text.unpack alertId'
  parseJSON o = typeMismatch "AlertId" o

instance ToJSON AlertId where
  toJSON (AlertId alertId') = toJSON alertId'

data AlertStatus = AlertStatusOk
                 | AlertStatusCritical
                 | AlertStatusWarning
                 | AlertStatusUnknown
                 deriving (Eq, Show)

$(deriveJSON options { constructorTagModifier = map toUpper . drop 11 } ''AlertStatus)

data Alert
  = Alert {
    alertId :: AlertId,
    alertStatus :: AlertStatus,
    alertMonitorId :: Maybe MonitorId,
    alertType :: MonitorType,
    alertHostId :: Maybe HostId,
    alertValue :: Maybe Double,
    alertMessage :: Maybe String,
    alertReason :: Maybe String,
    alertOpenedAt :: Integer,
    alertClosedAt :: Maybe Integer
  } deriving (Eq, Show)

$(deriveJSON options ''Alert)
