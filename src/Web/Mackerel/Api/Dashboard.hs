{-# LANGUAGE TemplateHaskell #-}
-- | Dashboard API.
module Web.Mackerel.Api.Dashboard
  ( listDashboards
  , getDashboard
  , createDashboard
  , updateDashboard
  , deleteDashboard
  ) where

import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)
import Network.Http.Client
import System.IO.Streams.ByteString (fromLazyByteString)

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
  = request client GET ("/api/v0/dashboards/" ++ dashboardId') [] emptyBody (createHandler id)

createDashboard :: Client -> DashboardCreate -> IO (Either ApiError Dashboard)
createDashboard client dashboard = do
  body <- inputStreamBody <$> fromLazyByteString (encode dashboard)
  request client POST "/api/v0/dashboards" [] body (createHandler id)

updateDashboard :: Client -> Dashboard -> IO (Either ApiError Dashboard)
updateDashboard client dashboard = do
  body <- inputStreamBody <$> fromLazyByteString (encode dashboard)
  let (DashboardId dashboardId') = dashboardId dashboard
  request client PUT ("/api/v0/dashboards/" ++ dashboardId') [] body (createHandler id)

deleteDashboard :: Client -> DashboardId -> IO (Either ApiError Dashboard)
deleteDashboard client (DashboardId dashboardId')
  = request client DELETE ("/api/v0/dashboards/" ++ dashboardId') [] emptyBody (createHandler id)
