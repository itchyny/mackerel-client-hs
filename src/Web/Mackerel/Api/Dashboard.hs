-- | Dashboard API.
module Web.Mackerel.Api.Dashboard
  ( listDashboards
  , getDashboard
  , createDashboard
  , updateDashboard
  , deleteDashboard
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Dashboard

data ListDashboardsResponse = ListDashboardsResponse { responseDashboards :: [Dashboard] }
$(deriveJSON options ''ListDashboardsResponse)

listDashboards :: Client -> IO (Either ApiError [Dashboard])
listDashboards client
  = request client GET "/api/v0/dashboards" [] emptyBody (createHandler responseDashboards)

getDashboard :: Client -> DashboardId -> IO (Either ApiError Dashboard)
getDashboard client (DashboardId dashboardId')
  = request client GET ("/api/v0/dashboards/" <> BS.pack dashboardId') [] emptyBody (createHandler id)

createDashboard :: Client -> DashboardCreate -> IO (Either ApiError Dashboard)
createDashboard client dashboard
  = request client POST "/api/v0/dashboards" [] (Just dashboard) (createHandler id)

updateDashboard :: Client -> Dashboard -> IO (Either ApiError Dashboard)
updateDashboard client dashboard = do
  let (DashboardId dashboardId') = dashboardId dashboard
  request client PUT ("/api/v0/dashboards/" <> BS.pack dashboardId') [] (Just dashboard) (createHandler id)

deleteDashboard :: Client -> DashboardId -> IO (Either ApiError Dashboard)
deleteDashboard client (DashboardId dashboardId')
  = request client DELETE ("/api/v0/dashboards/" <> BS.pack dashboardId') [] emptyBody (createHandler id)
