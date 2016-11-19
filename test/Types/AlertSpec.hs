{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.AlertSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import Data.Default
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types

spec :: Spec
spec = do

  let alert1 = Alert {
        alertId = AlertId "abcde0",
        alertStatus = AlertStatusCritical,
        alertMonitorId = Just "abcde2",
        alertType = AlertTypeConnectivity,
        alertHostId = Just $ HostId "abcde1",
        alertValue = Nothing,
        alertMessage = Nothing,
        alertReason = Nothing,
        alertOpenedAt = 1483196400,
        alertClosedAt = Nothing
      }

  let json1 = [aesonQQ|
    {
      "id": "abcde0",
      "status": "CRITICAL",
      "monitorId": "abcde2",
      "type": "connectivity",
      "hostId": "abcde1",
      "openedAt": 1483196400
    }
  |]

  let alert2 = Alert {
        alertId = AlertId "abcde0",
        alertStatus = AlertStatusWarning,
        alertMonitorId = Just "abcde2",
        alertType = AlertTypeHost,
        alertHostId = Just $ HostId "abcde1",
        alertValue = Just 25.0,
        alertMessage = Nothing,
        alertReason = Nothing,
        alertOpenedAt = 1483196400,
        alertClosedAt = Nothing
      }

  let json2 = [aesonQQ|
    {
      "id": "abcde0",
      "status": "WARNING",
      "monitorId": "abcde2",
      "type": "host",
      "hostId": "abcde1",
      "value": 25.0,
      "openedAt": 1483196400
    }
  |]

  let alert3 = Alert {
        alertId = AlertId "abcde0",
        alertStatus = AlertStatusCritical,
        alertMonitorId = Just "abcde2",
        alertType = AlertTypeExternal,
        alertHostId = Nothing,
        alertValue = Just 121,
        alertMessage = Just "SSL certification will expire within 696 days (2018-11-28T21:00:00+0900)",
        alertReason = Just "Close this alert",
        alertOpenedAt = 1483196400,
        alertClosedAt = Just 1483282800
      }

  let json3 = [aesonQQ|
    {
      "id": "abcde0",
      "status": "CRITICAL",
      "monitorId": "abcde2",
      "type": "external",
      "value": 121,
      "message": "SSL certification will expire within 696 days (2018-11-28T21:00:00+0900)",
      "reason": "Close this alert",
      "openedAt": 1483196400,
      "closedAt": 1483282800
    }
  |]

  describe "Alert FromJSON" $ do
    it "should parse a json" $
      forM_ [(alert1, json1), (alert2, json2), (alert3, json3)] $ \(alert, json) ->
        decode (encode json) `shouldBe` Just alert

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Alert)
      let (Object hm) = json1
      forM_ ["id", "status", "type", "openedAt"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Alert)

  describe "Alert ToJSON" $
    it "should encode into a json" $
      forM_ [(alert1, json1), (alert2, json2), (alert3, json3)] $ \(alert, json) ->
        decode (encode alert) `shouldBe` Just json
