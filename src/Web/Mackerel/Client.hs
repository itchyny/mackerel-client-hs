module Web.Mackerel.Client (Client, apiKey, apiBase, userAgent) where

import Data.Default (Default(..))

data Client
  = Client {
    apiKey :: String,
    apiBase :: String,
    userAgent :: String
  } deriving (Eq, Show)

instance Default Client where
  def = Client {
    apiKey = "",
    apiBase = "https://api.mackerelio.com",
    userAgent = "mackerel-client-hs"
  }
