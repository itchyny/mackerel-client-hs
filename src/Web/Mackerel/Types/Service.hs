module Web.Mackerel.Types.Service where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH

data Service
  = Service {
    serviceName :: String,
    serviceMemo :: String,
    serviceRoles :: [String]
  } deriving (Eq, Show)

$(deriveJSON options ''Service)
