{-# LANGUAGE TemplateHaskell #-}
-- | Monitor API.
module Web.Mackerel.Api.Monitor
  ( listMonitors
  , createMonitor
  , updateMonitor
  , deleteMonitor
  ) where

import Data.Aeson (encode, Value)
import Data.Aeson.TH (deriveJSON)
import Network.Http.Client
import System.IO.Streams.ByteString (fromLazyByteString)

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Monitor

data ListMonitorsResponse = ListMonitorsResponse { responseMonitors :: [Monitor] }
$(deriveJSON options ''ListMonitorsResponse)

listMonitors :: Client -> IO (Either ApiError [Monitor])
listMonitors client
  = request client GET "/api/v0/monitors" [] emptyBody (createHandler responseMonitors)

createMonitor :: Client -> Monitor -> IO (Either ApiError Monitor)
createMonitor client monitor = do
  body <- inputStreamBody <$> fromLazyByteString (encode monitor)
  request client POST "/api/v0/monitors" [] body (createHandler id)

data UpdateMonitorResponse = UpdateMonitorResponse { responseId :: MonitorId }
$(deriveJSON options ''UpdateMonitorResponse)

updateMonitor :: Client -> Monitor -> IO (Either ApiError MonitorId)
updateMonitor client monitor = do
  let Just (MonitorId monitorId') = monitorId monitor
  body <- inputStreamBody <$> fromLazyByteString (encode monitor)
  request client PUT ("/api/v0/monitors/" ++ monitorId') [] body (createHandler responseId)

deleteMonitor :: Client -> MonitorId -> IO (Either ApiError Value)
deleteMonitor client (MonitorId monitorId')
  = request client DELETE ("/api/v0/monitors/" ++ monitorId') [] emptyBody (createHandler id)
