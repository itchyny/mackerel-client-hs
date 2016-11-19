{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Host API.
module Web.Mackerel.Api.Host
  ( getHost
  , listHosts
  , ListHostsParams(..)
  , listHostMetricNames
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Default (Default(..))
import Data.Maybe (maybeToList)
import Network.Http.Client

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Host

data GetHostResponse = GetHostResponse { responseHost :: Host }
$(deriveJSON options ''GetHostResponse)

getHost :: Client -> HostId -> IO (Either ApiError Host)
getHost client (HostId hostId')
  = request client GET ("/api/v0/hosts/" ++ hostId') [] emptyBody (createHandler responseHost)

data ListHostsParams
  = ListHostsParams {
    listHostsParamsService :: Maybe String,
    listHostsParamsRoles :: [String],
    listHostsParamsName :: [String],
    listHostsParamsStatus :: [HostStatus]
  }

instance Default ListHostsParams where
  def = ListHostsParams Nothing [] [] [HostStatusWorking, HostStatusStandby, HostStatusMaintenance, HostStatusPoweroff]

data ListHostsResponse = ListHostsResponse { responseHosts :: [Host] }
$(deriveJSON options ''ListHostsResponse)

listHosts :: Client -> ListHostsParams -> IO (Either ApiError [Host])
listHosts client params = do
  let query = [ ("service", Just $ BS.pack s) | s <- maybeToList (listHostsParamsService params) ] ++
              [ ("role", Just $ BS.pack r) | r <- listHostsParamsRoles params ] ++
              [ ("name", Just $ BS.pack r) | r <- listHostsParamsName params ] ++
              [ ("status", Just $ BS.pack $ show s) | s <- listHostsParamsStatus params ]
  request client GET "/api/v0/hosts" query emptyBody (createHandler responseHosts)

data ListMetricNamesResponse = ListMetricNamesResponse { responseNames :: [String] }
$(deriveJSON options ''ListMetricNamesResponse)

listHostMetricNames :: Client -> HostId -> IO (Either ApiError [String])
listHostMetricNames client (HostId hostId')
  = request client GET ("/api/v0/hosts/" ++ hostId' ++ "/metric-names") [] emptyBody (createHandler responseNames)
