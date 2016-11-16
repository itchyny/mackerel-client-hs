{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Organization where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH

data Organization = Organization { organizationName :: String }
                  deriving (Eq, Show)

$(deriveJSON options ''Organization)
