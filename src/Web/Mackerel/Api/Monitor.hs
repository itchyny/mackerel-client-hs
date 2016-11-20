{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Monitor API.
module Web.Mackerel.Api.Monitor
  ( listMonitors
  , createMonitor
  , updateMonitor
  , deleteMonitor
  ) where

import Data.Aeson (encode, Value)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Semigroup ((<>))
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Monitor

data ListMonitorsResponse = ListMonitorsResponse { responseMonitors :: [Monitor] }
$(deriveJSON options ''ListMonitorsResponse)

listMonitors :: Client -> IO (Either ApiError [Monitor])
listMonitors client
  = request client GET "/api/v0/monitors" [] "" (createHandler responseMonitors)

createMonitor :: Client -> Monitor -> IO (Either ApiError Monitor)
createMonitor client monitor
  = request client POST "/api/v0/monitors" [] (encode monitor) (createHandler id)

data UpdateMonitorResponse = UpdateMonitorResponse { responseId :: MonitorId }
$(deriveJSON options ''UpdateMonitorResponse)

updateMonitor :: Client -> Monitor -> IO (Either ApiError MonitorId)
updateMonitor client monitor = do
  let Just (MonitorId monitorId') = monitorId monitor
  request client PUT ("/api/v0/monitors/" <> BS.pack monitorId') [] (encode monitor) (createHandler responseId)

deleteMonitor :: Client -> MonitorId -> IO (Either ApiError Value)
deleteMonitor client (MonitorId monitorId')
  = request client DELETE ("/api/v0/monitors/" <> BS.pack monitorId') [] "" (createHandler id)
