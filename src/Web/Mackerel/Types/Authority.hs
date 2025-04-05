module Web.Mackerel.Types.Authority where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text

data Authority = AuthorityOwner
               | AuthorityManager
               | AuthorityCollaborator
               | AuthorityViewer
               deriving Eq

instance Show Authority where
  show AuthorityOwner = "owner"
  show AuthorityManager = "manager"
  show AuthorityCollaborator = "collaborator"
  show AuthorityViewer = "viewer"

instance Read Authority where
  readsPrec _ xs = [ (hs, drop (length str) xs) | (hs, str) <- pairs', take (length str) xs == str ]
    where pairs' = [
            (AuthorityOwner, "owner"),
            (AuthorityManager, "manager"),
            (AuthorityCollaborator, "collaborator"),
            (AuthorityViewer, "viewer")]

instance FromJSON Authority where
  parseJSON (Aeson.String txt)
    | str == "owner" = return AuthorityOwner
    | str == "manager" = return AuthorityManager
    | str == "collaborator" = return AuthorityCollaborator
    | str == "viewer" = return AuthorityViewer
    where str = Text.unpack txt
  parseJSON o = typeMismatch "Authority" o

instance ToJSON Authority where
  toJSON method = toJSON (show method)
