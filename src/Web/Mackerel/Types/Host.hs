{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Mackerel.Types.Host where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, constructorTagModifier, fieldLabelModifier)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Char (toLower)
import Data.Default (Default(..))
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (isJust)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data HostId = HostId String
            deriving (Eq, Show)

instance FromJSON HostId where
  parseJSON (Aeson.String hostId) = return $ HostId $ Text.unpack hostId
  parseJSON o = typeMismatch "HostId" o

instance ToJSON HostId where
  toJSON (HostId hostId) = toJSON hostId

data HostMetaCloud
  = HostMetaCloud {
    metaCloudProvider :: String,
    metaCloudMetadata :: HM.HashMap String Value
  } deriving (Eq, Show)

$(deriveJSON options { fieldLabelModifier = map toLower . drop 9 } ''HostMetaCloud)

data HostMetaCpu
  = HostMetaCpu {
    metaCpuCacheSize :: Maybe String,
    metaCpuCoreId :: Maybe String,
    metaCpuCores :: Maybe String,
    metaCpuFamily :: Maybe String,
    metaCpuMhz :: Maybe String,
    metaCpuModel :: Maybe String,
    metaCpuModelName :: Maybe String,
    metaCpuPhysicalId :: Maybe String,
    metaCpuStepping :: Maybe String,
    metaCpuVendorId :: Maybe String
  } deriving (Eq, Show)

instance FromJSON HostMetaCpu where
  parseJSON (Object o)
    = HostMetaCpu
      <$> o .:? "cache_size" <*> o .:? "core_id" <*> o .:? "cores" <*> o .:? "family"
      <*> (o .:? "mhz" <|> fmap show <$> (o .:? "mhz" :: Parser (Maybe Integer)))
      <*> (o .:? "model" <|> fmap show <$> (o .:? "model" :: Parser (Maybe Integer)))
      <*> o .:? "model_name" <*> o .:? "physical_id" <*> o .:? "stepping" <*> o .:? "vendor_id"
  parseJSON o = typeMismatch "HostMetaCpu" o

instance ToJSON HostMetaCpu where
  toJSON (HostMetaCpu cacheSize coreId cores family mhz model modelName physicalId stepping vendorId)
    = object [ key .= value |
        (key, value) <- [
          ("cache_size", cacheSize), ("core_id", coreId), ("cores", cores), ("family", family),
          ("mhz", mhz), ("model", model), ("model_name", modelName),
          ("physical_id", physicalId), ("stepping", stepping), ("vendor_id", vendorId)
        ],
        isJust value
    ]

data HostMeta
  = HostMeta {
    metaAgentName :: Maybe String,
    metaAgentRevision :: Maybe String,
    metaAgentVersion :: Maybe String,
    metaBlockDevice :: Maybe (HM.HashMap String (HM.HashMap String String)),
    metaCpu :: Maybe [HostMetaCpu],
    metaFilesystem :: Maybe (HM.HashMap String (HM.HashMap String Value)),
    metaKernel :: Maybe (HM.HashMap String String),
    metaMemory :: Maybe (HM.HashMap String String),
    metaCloud :: Maybe HostMetaCloud
  } deriving (Eq, Show)

instance Default HostMeta where
  def = HostMeta def def def def def def def def def

$(deriveJSON options {
  fieldLabelModifier
    = \xs -> let ys = drop 4 xs in if take 5 ys == "Agent" then kebabCase ys else snakeCase ys
} ''HostMeta)

data HostStatus = HostStatusWorking
                | HostStatusStandby
                | HostStatusMaintenance
                | HostStatusPoweroff
                deriving Eq

instance Show HostStatus where
  show HostStatusWorking = "working"
  show HostStatusStandby = "standby"
  show HostStatusMaintenance = "maintenance"
  show HostStatusPoweroff = "poweroff"

instance Read HostStatus where
  readsPrec _ xs = [ (hs, drop (length str) xs) | (hs, str) <- pairs', take (length str) xs == str ]
    where pairs' = [(HostStatusWorking, "working"), (HostStatusStandby, "standby"), (HostStatusMaintenance, "maintenance"), (HostStatusPoweroff, "poweroff")]

$(deriveJSON options { constructorTagModifier = map toLower . drop 10 } ''HostStatus)

data HostInterface
  = HostInterface {
    hostInterfaceName :: String,
    hostInterfaceMacAddress :: Maybe String,
    hostInterfaceIpv4Addresses :: Maybe [String],
    hostInterfaceIpv6Addresses :: Maybe [String]
  } deriving (Eq, Show)

$(deriveJSON options { fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 13 } ''HostInterface)

data Host
  = Host {
    hostId :: HostId,
    hostName :: String,
    hostDisplayName :: Maybe String,
    hostStatus :: HostStatus,
    hostMemo :: String,
    hostRoles :: HM.HashMap String [String],
    hostIsRetired :: Bool,
    hostCreatedAt :: Integer,
    hostMeta :: HostMeta,
    hostInterfaces :: [HostInterface]
  } deriving (Eq, Show)

$(deriveJSON options ''Host)

data HostCreate
  = HostCreate {
    hostCreateName :: String,
    hostCreateDisplayName :: Maybe String,
    hostCreateCustomIdentifier :: Maybe String,
    hostCreateMeta :: HostMeta,
    hostCreateInterfaces :: Maybe [HostInterface],
    hostCreateRoleFullnames :: Maybe [String],
    hostCreateChecks :: Maybe [String]
  } deriving (Eq, Show)

instance Default HostCreate where
  def = HostCreate def def def def def def def

$(deriveJSON options { fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 10 } ''HostCreate)
