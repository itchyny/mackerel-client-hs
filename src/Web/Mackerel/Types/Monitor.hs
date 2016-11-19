{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Monitor where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, SumEncoding(..), sumEncoding, constructorTagModifier, fieldLabelModifier)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data MonitorId = MonitorId String
               deriving (Eq, Show)

instance FromJSON MonitorId where
  parseJSON (Aeson.String monitorId') = return $ MonitorId $ Text.unpack monitorId'
  parseJSON o = typeMismatch "MonitorId" o

instance ToJSON MonitorId where
  toJSON (MonitorId monitorId') = toJSON monitorId'

data MonitorOperator = MonitorGreaterThan | MonitorLessThan
                     deriving Eq

instance Show MonitorOperator where
  show MonitorGreaterThan = ">"
  show MonitorLessThan = "<"

instance Read MonitorOperator where
  readsPrec _ xs = [ (op, drop 1 xs) | (op, opstr) <- [(MonitorGreaterThan, ">"), (MonitorLessThan, "<")], take 1 xs == opstr ]

instance FromJSON MonitorOperator where
  parseJSON (Aeson.String str)
    | Text.unpack str == ">" = return MonitorGreaterThan
    | Text.unpack str == "<" = return MonitorLessThan
  parseJSON o = typeMismatch "MonitorOperator" o

instance ToJSON MonitorOperator where
  toJSON op = toJSON (show op)

data MonitorExternalHeader
  = MonitorExternalHeader {
    monitorHeaderName :: String,
    monitorHeaderValue :: String
  } deriving (Eq, Show)

$(deriveJSON options { fieldLabelModifier = map toLower . drop 13 } ''MonitorExternalHeader)

data Monitor
  = MonitorHost {
    monitorId :: Maybe MonitorId,
    monitorName :: String,
    monitorDuration :: Integer,
    monitorMetric :: String,
    monitorOperator :: MonitorOperator,
    monitorWarning :: Double,
    monitorCritical :: Double,
    monitorIsMute :: Maybe Bool,
    monitorNotificationInterval :: Maybe Integer,
    monitorScopes :: Maybe [String],
    monitorExcludeScopes :: Maybe [String]
  }
  | MonitorConnectivity {
    monitorId :: Maybe MonitorId,
    monitorName :: String,
    monitorIsMute :: Maybe Bool,
    monitorNotificationInterval :: Maybe Integer,
    monitorScopes :: Maybe [String],
    monitorExcludeScopes :: Maybe [String]
  }
  | MonitorService {
    monitorId :: Maybe MonitorId,
    monitorName :: String,
    monitorService :: String,
    monitorDuration :: Integer,
    monitorMetric :: String,
    monitorOperator :: MonitorOperator,
    monitorWarning :: Double,
    monitorCritical :: Double,
    monitorIsMute :: Maybe Bool,
    monitorNotificationInterval :: Maybe Integer
  }
  | MonitorExternal {
    monitorId :: Maybe MonitorId,
    monitorName :: String,
    monitorUrl :: String,
    monitorServiceOpt :: Maybe String,
    monitorResponseTimeDuration :: Maybe Double,
    monitorResponseTimeWarning :: Maybe Double,
    monitorResponseTimeCritical :: Maybe Double,
    monitorContainsString :: Maybe String,
    monitorMaxCheckAttempts :: Maybe Integer,
    monitorCertificationExpirationWarning :: Maybe Integer,
    monitorCertificationExpirationCritical :: Maybe Integer,
    monitorSkipCertificateVerification :: Maybe Bool,
    monitorHeaders :: Maybe [MonitorExternalHeader],
    monitorIsMute :: Maybe Bool,
    monitorNotificationInterval :: Maybe Integer
  }
  | MonitorExpression {
    monitorId :: Maybe MonitorId,
    monitorName :: String,
    monitorExpression :: String,
    monitorOperator :: MonitorOperator,
    monitorWarning :: Double,
    monitorCritical :: Double,
    monitorIsMute :: Maybe Bool,
    monitorNotificationInterval :: Maybe Integer
  } deriving (Eq, Show)

$(deriveJSON options {
  sumEncoding = TaggedObject "type" "type",
  constructorTagModifier = map toLower . drop 7,
  fieldLabelModifier = \xs ->
    let ys = fieldLabelModifier options xs
        in if "Opt" `isSuffixOf` ys then take (length ys - 3) ys else ys
} ''Monitor)
