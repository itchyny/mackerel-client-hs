{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Host where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, constructorTagModifier, fieldLabelModifier)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import Data.Default (Default(..))
import qualified Data.HashMap.Lazy as HM
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

$(deriveJSON options {
  fieldLabelModifier = map toLower . drop (length "metaCloud")
} ''HostMetaCloud)

data HostMeta
  = HostMeta {
    metaAgentName :: Maybe String,
    metaAgentRevision :: Maybe String,
    metaAgentVersion :: Maybe String,
    metaBlockDevice :: Maybe (HM.HashMap String (HM.HashMap String String)),
    metaCpu :: Maybe [HM.HashMap String String],
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
                deriving (Eq, Show)

$(deriveJSON options {
  constructorTagModifier = map toLower . drop (length "HostStatus")
} ''HostStatus)

data HostInterface
  = HostInterface {
    hostInterfaceName :: String,
    hostInterfaceMacAddress :: Maybe String,
    hostInterfaceIpv4Addresses :: Maybe [String],
    hostInterfaceIpv6Addresses :: Maybe [String]
  } deriving (Eq, Show)

$(deriveJSON options {
  fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop (length "hostInterface")
} ''HostInterface)

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
