{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Invitation where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data InvitationAuthority = InvitationAuthorityManager
                         | InvitationAuthorityCollaborator
                         | InvitationAuthorityViewer
                         deriving Eq

instance Show InvitationAuthority where
  show InvitationAuthorityManager = "manager"
  show InvitationAuthorityCollaborator = "collaborator"
  show InvitationAuthorityViewer = "viewer"

instance Read InvitationAuthority where
  readsPrec _ xs = [ (hs, drop (length str) xs) | (hs, str) <- pairs', take (length str) xs == str ]
    where pairs' = [(InvitationAuthorityManager, "manager"), (InvitationAuthorityCollaborator, "collaborator"), (InvitationAuthorityViewer, "viewer")]

instance FromJSON InvitationAuthority where
  parseJSON (Aeson.String txt)
    | str == "manager" = return InvitationAuthorityManager
    | str == "collaborator" = return InvitationAuthorityCollaborator
    | str == "viewer" = return InvitationAuthorityViewer
    where str = Text.unpack txt
  parseJSON o = typeMismatch "InvitationAuthority" o

instance ToJSON InvitationAuthority where
  toJSON method = toJSON (show method)

data Invitation
  = Invitation {
    invitationEmail :: String,
    invitationAuthority :: InvitationAuthority
  } deriving (Eq, Show)

$(deriveJSON options ''Invitation)
