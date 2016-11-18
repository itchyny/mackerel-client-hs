{-# LANGUAGE TemplateHaskell #-}
-- | Alert API.
module Web.Mackerel.Api.Alert (listAlerts, closeAlert) where

import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.HashMap.Lazy as HM
import Network.Http.Client
import System.IO.Streams.ByteString (fromLazyByteString)

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
  body <- inputStreamBody <$> fromLazyByteString (encode (HM.fromList [("reason", reason)]))
  request client POST ("/api/v0/alerts/" ++ alertId' ++ "/close") [] body (createHandler id)
