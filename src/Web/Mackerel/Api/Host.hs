{-# LANGUAGE TemplateHaskell #-}
-- | Host API.
module Web.Mackerel.Api.Host
  ( getHost
  , listHosts
  , listHostMetricNames
  ) where

import Data.Aeson.TH (deriveJSON)
import Network.Http.Client

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Host

data GetHostResponse = GetHostResponse { responseHost :: Host }
$(deriveJSON options ''GetHostResponse)

getHost :: Client -> HostId -> IO (Either ApiError Host)
getHost client (HostId hostId')
  = request client GET ("/api/v0/hosts/" ++ hostId') emptyBody (createHandler responseHost)

data ListHostsResponse = ListHostsResponse { responseHosts :: [Host] }
$(deriveJSON options ''ListHostsResponse)

listHosts :: Client -> IO (Either ApiError [Host])
listHosts client
  = request client GET "/api/v0/hosts" emptyBody (createHandler responseHosts)

data ListMetricNamesResponse = ListMetricNamesResponse { responseNames :: [String] }
$(deriveJSON options ''ListMetricNamesResponse)

listHostMetricNames :: Client -> HostId -> IO (Either ApiError [String])
listHostMetricNames client (HostId hostId')
  = request client GET ("/api/v0/hosts/" ++ hostId' ++ "/metric-names") emptyBody (createHandler responseNames)
