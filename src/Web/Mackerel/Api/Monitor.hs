-- | Monitor API.
module Web.Mackerel.Api.Monitor
  ( listMonitors
  , createMonitor
  , updateMonitor
  , deleteMonitor
  ) where

import Data.Aeson (Value)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

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
createMonitor client monitor
  = request client POST "/api/v0/monitors" [] (Just monitor) (createHandler id)

updateMonitor :: Client -> Monitor -> IO (Either ApiError Monitor)
updateMonitor client monitor = do
  let Just (MonitorId monitorId') = monitorId monitor
  request client PUT ("/api/v0/monitors/" <> BS.pack monitorId') [] (Just monitor) (createHandler id)

deleteMonitor :: Client -> MonitorId -> IO (Either ApiError Value)
deleteMonitor client (MonitorId monitorId')
  = request client DELETE ("/api/v0/monitors/" <> BS.pack monitorId') [] emptyBody (createHandler id)
