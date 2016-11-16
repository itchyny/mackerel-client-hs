{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.User where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH

data User
  = User {
    userId :: String,
    userScreenName :: String,
    userEmail :: String
  } deriving (Eq, Show)

$(deriveJSON options ''User)
