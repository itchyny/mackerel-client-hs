{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Invitation where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Authority

data Invitation
  = Invitation {
    invitationEmail :: String,
    invitationAuthority :: Authority
  } deriving (Eq, Show)

$(deriveJSON options ''Invitation)
