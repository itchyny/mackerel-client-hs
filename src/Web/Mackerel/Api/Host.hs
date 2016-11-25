{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Host API.
module Web.Mackerel.Api.Host
  ( createHost
  , getHost
  , updateHost
  , updateHostStatus
  , listHosts
  , ListHostsParams(..)
  , listHostMetricNames
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Default (Default(..))
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (maybeToList)
import Data.Semigroup ((<>))
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Host

data CreateHostResponse = CreateHostResponse { responseId :: HostId }
$(deriveJSON options ''CreateHostResponse)

createHost :: Client -> HostCreate -> IO (Either ApiError HostId)
createHost client host
  = request client POST "/api/v0/hosts" [] (Just host) (createHandler responseId)

data GetHostResponse = GetHostResponse { responseHost :: Host }
$(deriveJSON options ''GetHostResponse)

getHost :: Client -> HostId -> IO (Either ApiError Host)
getHost client (HostId hostId')
  = request client GET ("/api/v0/hosts/" <> BS.pack hostId') [] emptyBody (createHandler responseHost)

updateHost :: Client -> HostId -> HostCreate -> IO (Either ApiError HostId)
updateHost client (HostId hostId') host
  = request client PUT ("/api/v0/hosts/" <> BS.pack hostId') [] (Just host) (createHandler responseId)

data SuccessResponse = SuccessResponse { responseSuccess :: Bool }
$(deriveJSON options ''SuccessResponse)

updateHostStatus :: Client -> HostId -> HostStatus -> IO (Either ApiError Bool)
updateHostStatus client (HostId hostId') status = do
  let body = Just $ HM.fromList [("status", status) :: (String, HostStatus)]
  request client POST ("/api/v0/hosts/" <> BS.pack hostId' <> "/status") [] body (createHandler responseSuccess)

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
  = request client GET ("/api/v0/hosts/" <> BS.pack hostId' <> "/metric-names") [] emptyBody (createHandler responseNames)
