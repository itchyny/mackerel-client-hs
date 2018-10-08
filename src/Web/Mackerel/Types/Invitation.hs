{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Invitation where

import Data.Aeson.TH (deriveJSON, fieldLabelModifier)
import Data.Char (toLower)

import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Authority

data Invitation
  = Invitation {
    invitationEmail :: String,
    invitationAuthority :: Authority,
    invitationExpiresAt :: Integer
  } deriving (Eq, Show)

$(deriveJSON options ''Invitation)

data InvitationCreate
  = InvitationCreate {
    invitationCreateEmail :: String,
    invitationCreateAuthority :: Authority
  } deriving (Eq, Show)

$(deriveJSON options { fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 16 } ''InvitationCreate)
