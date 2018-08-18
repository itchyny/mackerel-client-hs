{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Service API.
module Web.Mackerel.Api.Service
  ( listServices
  , createService
  , deleteService
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

createService :: Client -> Service -> IO (Either ApiError Service)
createService client service
  = request client POST "/api/v0/services" [] (Just service) (createHandler id)

deleteService :: Client -> String -> IO (Either ApiError Service)
deleteService client name
  = request client DELETE ("/api/v0/services/" <> BS.pack name) [] emptyBody (createHandler id)

data ListMetricNamesResponse = ListMetricNamesResponse { responseNames :: [String] }
$(deriveJSON options ''ListMetricNamesResponse)

listServiceMetricNames :: Client -> String -> IO (Either ApiError [String])
listServiceMetricNames client serviceName'
  = request client GET ("/api/v0/services/" <> BS.pack serviceName' <> "/metric-names") [] emptyBody (createHandler responseNames)
