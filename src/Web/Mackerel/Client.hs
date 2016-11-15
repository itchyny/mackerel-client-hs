module Web.Mackerel.Client (Client(..)) where

import Data.Default (Default(..))
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)

data Client
  = Client {
    apiKey :: String,
    apiBase :: URI,
    userAgent :: String
  }

instance Default Client where
  def = Client {
    apiKey = "",
    apiBase = fromJust $ parseURI "https://mackerel.io",
    userAgent = "mackerel-client-hs"
  }
