{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.MonitorSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode)
import Data.Aeson.QQ
import Test.Hspec

import Web.Mackerel.Types.Monitor

spec :: Spec
spec = do

  let hostMonitor = MonitorHost {
        monitorId = Just $ MonitorId "abcde1",
        monitorName = "Monitor custom.foo.bar",
        monitorDuration = 5,
        monitorMetric = "custom.foo.bar",
        monitorOperator = MonitorGreaterThan,
        monitorWarning = 10.0,
        monitorCritical = 20.0,
        monitorIsMute = Just False,
        monitorNotificationInterval = Just 30,
        monitorScopes = Just ["service0"],
        monitorExcludeScopes = Just ["service0:role3"]
      }

  let hostMonitorJson = [aesonQQ|
    {
      "type": "host",
      "id": "abcde1",
      "name": "Monitor custom.foo.bar",
      "duration": 5,
      "metric": "custom.foo.bar",
      "operator": ">",
      "warning": 10.0,
      "critical": 20.0,
      "isMute": false,
      "notificationInterval": 30,
      "scopes": ["service0"],
      "excludeScopes": ["service0:role3"]
    }
  |]

  let connectivity = MonitorConnectivity {
        monitorId = Just $ MonitorId "abcde2",
        monitorName = "connectivity",
        monitorIsMute = Just False,
        monitorNotificationInterval = Nothing,
        monitorScopes = Nothing,
        monitorExcludeScopes = Nothing
      }

  let connectivityJson = [aesonQQ|
    {
      "type": "connectivity",
      "id": "abcde2",
      "name": "connectivity",
      "isMute": false
    }
  |]

  let serviceMonitor = MonitorService {
        monitorId = Just $ MonitorId "abcde3",
        monitorName = "Service count",
        monitorService = "service1",
        monitorDuration = 5,
        monitorMetric = "custom.service.count",
        monitorOperator = MonitorGreaterThan,
        monitorWarning = 100,
        monitorCritical = 200,
        monitorIsMute = Just False,
        monitorNotificationInterval = Just 30
      }

  let serviceMonitorJson = [aesonQQ|
    {
      "type": "service",
      "id": "abcde3",
      "name": "Service count",
      "service": "service1",
      "duration": 5,
      "metric": "custom.service.count",
      "operator": ">",
      "warning": 100,
      "critical": 200,
      "isMute": false,
      "notificationInterval": 30
    }
  |]

  let externalMonitor = MonitorExternal {
        monitorId = Just $ MonitorId "abcde4",
        monitorName = "Example external monitor",
        monitorUrl = "https://example.com",
        monitorServiceOption = Just "service1",
        monitorResponseTimeDuration = Just 5,
        monitorResponseTimeWarning = Just 3000,
        monitorResponseTimeCritical = Just 5000,
        monitorContainsString = Just "Example Domain",
        monitorMaxCheckAttempts = Just 5,
        monitorCertificationExpirationWarning = Just 1200,
        monitorCertificationExpirationCritical = Just 60,
        monitorSkipCertificateVerification = Just True,
        monitorHeaders = Just [MonitorExternalHeader "Cache-Control" "no-cache"],
        monitorIsMute = Just True,
        monitorNotificationInterval = Just 60
      }

  let externalMonitorJson = [aesonQQ|
    {
      "type": "external",
      "id": "abcde4",
      "name": "Example external monitor",
      "url": "https://example.com",
      "service": "service1",
      "responseTimeDuration": 5,
      "responseTimeWarning": 3000,
      "responseTimeCritical": 5000,
      "containsString": "Example Domain",
      "maxCheckAttempts": 5,
      "certificationExpirationWarning": 1200,
      "certificationExpirationCritical": 60,
      "skipCertificateVerification": true,
      "headers": [{ "name": "Cache-Control", "value": "no-cache" }],
      "isMute": true,
      "notificationInterval": 60
    }
  |]

  let expressionMonitor = MonitorExpression {
        monitorId = Just $ MonitorId "abcde5",
        monitorName = "Example expression monitor",
        monitorExpression = "min(role(\"foo:bar\", \"custom.foo.bar\"))",
        monitorOperator = MonitorLessThan,
        monitorWarning = 10.0,
        monitorCritical = 20.0,
        monitorIsMute = Just False,
        monitorNotificationInterval = Nothing
      }

  let expressionMonitorJson = [aesonQQ|
    {
      "type": "expression",
      "id": "abcde5",
      "name": "Example expression monitor",
      "expression": "min(role(\"foo:bar\", \"custom.foo.bar\"))",
      "operator": "<",
      "warning": 10.0,
      "critical": 20.0,
      "isMute": false
    }
  |]

  let monitors = [
          (hostMonitor, hostMonitorJson),
          (connectivity, connectivityJson),
          (serviceMonitor, serviceMonitorJson),
          (externalMonitor, externalMonitorJson),
          (expressionMonitor, expressionMonitorJson)
        ]

  describe "Monitor FromJSON" $
    it "should parse jsons" $ do
      forM_ monitors $ \(monitor, json) ->
        decode (encode json) `shouldBe` Just monitor
      decode (encode (map snd monitors)) `shouldBe` Just (map fst monitors)

  describe "Monitor ToJSON" $
    it "should encode into jsons" $ do
      forM_ monitors $ \(monitor, json) ->
        decode (encode monitor) `shouldBe` Just json
      decode (encode (map fst monitors)) `shouldBe` Just (map snd monitors)
