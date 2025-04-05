-- | Alert API.
module Web.Mackerel.Api.Alert (listAlerts, closeAlert) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Lazy qualified as HM
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Alert

data ListAlertsResponse = ListAlertsResponse { responseAlerts :: [Alert] }
$(deriveJSON options ''ListAlertsResponse)

listAlerts :: Client -> IO (Either ApiError [Alert])
listAlerts client
  = request client GET "/api/v0/alerts" [] emptyBody (createHandler responseAlerts)

closeAlert :: Client -> AlertId -> String -> IO (Either ApiError Alert)
closeAlert client (AlertId alertId') reason = do
  let body = Just $ HM.fromList [("reason", reason) :: (String, String)]
  request client POST ("/api/v0/alerts/" <> BS.pack alertId' <> "/close") [] body (createHandler id)
