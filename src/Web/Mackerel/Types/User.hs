module Web.Mackerel.Types.User where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Authority

data UserId = UserId String
            deriving (Eq, Show)

instance FromJSON UserId where
  parseJSON (Aeson.String alertId) = return $ UserId $ Text.unpack alertId
  parseJSON o = typeMismatch "UserId" o

instance ToJSON UserId where
  toJSON (UserId alertId) = toJSON alertId

data User
  = User {
    userId :: UserId,
    userScreenName :: String,
    userEmail :: String,
    userAuthority :: Authority
  } deriving (Eq, Show)

$(deriveJSON options ''User)
