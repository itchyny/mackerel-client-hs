module Web.Mackerel.Types.Role where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH

data Role
  = Role {
    roleName :: String,
    roleMemo :: String
  } deriving (Eq, Show)

$(deriveJSON options ''Role)
