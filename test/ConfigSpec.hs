{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ConfigSpec where

import Control.Monad (forM_)
import Data.Default
import Test.Hspec
import Text.RawString.QQ

import Web.Mackerel.Config
import Web.Mackerel.Types.Host

spec :: Spec
spec = do

  let config1 = def {
        configApiKey = Just "<Mackerel-API-KEY>"
      }

  let toml1 = [r|
        apikey = "<Mackerel-API-KEY>"
      |]

  let config2 = Config {
        configApiBase = Just "https://mackerel.io",
        configApiKey = Just "<Mackerel-API-KEY>",
        configRoot = Just "/var/lib/mackerel-agent",
        configPidfile = Just "/var/run/mackerel-agent.pid",
        configRoles = Just ["service1:role1", "service2:role3"],
        configVerbose = Just True,
        configDiagnostic = Just False,
        configDisplayname = Just "Example Host",
        configHostStatus = Just HostStatusConfig {
          hostStatusOnStart = Just HostStatusWorking,
          hostStatusOnStop = Just HostStatusPoweroff
        },
        configHttpProxy = Just "http://localhost:8080"
      }

  let toml2 = [r|
        apibase = "https://mackerel.io"
        apikey = "<Mackerel-API-KEY>"
        root = "/var/lib/mackerel-agent"
        pidfile = "/var/run/mackerel-agent.pid"

        verbose = true
        diagnostic = false

        display_name = "Example Host"
        http_proxy = "http://localhost:8080"

        roles = [ "service1:role1", "service2:role3" ]

        include = "/etc/mackerel-agent/conf.d/*.conf"

        [host_status]
        on_start = "working"
        on_stop  = "poweroff"
      |]

  describe "parseConfig" $
    it "should parse a configuration toml" $
      forM_ [(toml1, config1), (toml2, config2)] $ \(toml, config) ->
        parseConfig toml `shouldBe` Right config
