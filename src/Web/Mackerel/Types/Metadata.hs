{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.Metadata where

import Data.Aeson.TH (deriveJSON)

import Web.Mackerel.Internal.TH

data Metadata
  = Metadata {
    metadataNamespace :: String
  } deriving (Eq, Show)

$(deriveJSON options ''Metadata)
