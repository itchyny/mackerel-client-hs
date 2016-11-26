# mackerel-client-hs [![Build Status](https://travis-ci.org/itchyny/mackerel-client-hs.png?branch=master)](https://travis-ci.org/itchyny/mackerel-client-hs)
An API client library for [Mackerel](https://mackerel.io).

API documents: [Mackerel API Documents (v0)](https://mackerel.io/api-docs/)

The official Go client library: [mackerel-client-go](https://github.com/mackerelio/mackerel-client-go)

## Example
```haskell
import Data.Default
import Web.Mackerel

main :: IO ()
main = do
  let client = def { apiKey = "<Mackerel-API-KEY>" }

  print =<< getOrganization client
  print =<< listUsers client

  print =<< listHosts client def { listHostsParamsService = Just "servicename", listHostsParamsRoles = ["role1", "role2"] }

  print =<< listMonitors client
  print =<< updateMonitor client monitor { monitorName = "Monitor name renamed" }
  print =<< deleteMonitor client (MonitorId "<Monitor-ID>")

  print =<< listAlerts client
  print =<< closeAlert client (AlertId "<Alert-ID>") "this is not an important alert"
```

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
