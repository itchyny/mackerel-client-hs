{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Service API.
module Web.Mackerel.Api.Service
  ( listServices
  , listServiceMetricNames
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Semigroup ((<>))
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Service

data ListServicesResponse = ListServicesResponse { responseServices :: [Service] }
$(deriveJSON options ''ListServicesResponse)

listServices :: Client -> IO (Either ApiError [Service])
listServices client
  = request client GET "/api/v0/services" [] emptyBody (createHandler responseServices)

data ListMetricNamesResponse = ListMetricNamesResponse { responseNames :: [String] }
$(deriveJSON options ''ListMetricNamesResponse)

listServiceMetricNames :: Client -> String -> IO (Either ApiError [String])
listServiceMetricNames client serviceName'
  = request client GET ("/api/v0/services/" <> BS.pack serviceName' <> "/metric-names") [] emptyBody (createHandler responseNames)
