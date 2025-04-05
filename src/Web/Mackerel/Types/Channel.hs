module Web.Mackerel.Types.Channel where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data ChannelId = ChannelId String
               deriving (Eq, Show)

instance FromJSON ChannelId where
  parseJSON (Aeson.String channelId') = return $ ChannelId $ Text.unpack channelId'
  parseJSON o = typeMismatch "ChannelId" o

instance ToJSON ChannelId where
  toJSON (ChannelId channelId') = toJSON channelId'

data Channel
  = Channel {
    channelId :: ChannelId,
    channelName :: String,
    channelType :: String
  } deriving (Eq, Show)

$(deriveJSON options ''Channel)
