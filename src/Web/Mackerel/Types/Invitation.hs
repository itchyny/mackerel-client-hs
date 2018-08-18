{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Invitation where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Authority

data Invitation
  = Invitation {
    invitationEmail :: String,
    invitationAuthority :: Authority
  } deriving (Eq, Show)

$(deriveJSON options ''Invitation)
