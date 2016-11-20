module Web.Mackerel.Client (Client, apiKey, apiBase, userAgent) where

import Data.Default (Default(..))

data Client
  = Client {
    apiKey :: String,
    apiBase :: String,
    userAgent :: String
  }

instance Default Client where
  def = Client {
    apiKey = "",
    apiBase = "https://mackerel.io",
    userAgent = "mackerel-client-hs"
  }
