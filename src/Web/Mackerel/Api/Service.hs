{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Service API.
module Web.Mackerel.Api.Service
  ( listServices
  , listServiceMetricNames
  ) where

import Data.Aeson.TH (deriveJSON)
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Service

data ListServicesResponse = ListServicesResponse { responseServices :: [Service] }
$(deriveJSON options ''ListServicesResponse)

listServices :: Client -> IO (Either ApiError [Service])
listServices client
  = request client GET "/api/v0/services" [] "" (createHandler responseServices)

data ListMetricNamesResponse = ListMetricNamesResponse { responseNames :: [String] }
$(deriveJSON options ''ListMetricNamesResponse)

listServiceMetricNames :: Client -> String -> IO (Either ApiError [String])
listServiceMetricNames client serviceName'
  = request client GET ("/api/v0/services/" ++ serviceName' ++ "/metric-names") [] "" (createHandler responseNames)
