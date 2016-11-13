-- | Helpers for deriveJSON.
module Web.Mackerel.Internal.TH where

import Data.Aeson.TH (Options(..), defaultOptions)
import Data.Char (isLower, toLower)

options :: Options
options = defaultOptions {
  fieldLabelModifier = (\(c:cs) -> toLower c : cs) . dropWhile isLower,
  constructorTagModifier = map toLower,
  omitNothingFields = True
}
