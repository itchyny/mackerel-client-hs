-- | Helpers for deriveJSON.
module Web.Mackerel.Internal.TH (options, kebabCase, snakeCase) where

import Data.Aeson.TH (Options(..), defaultOptions)
import Data.Char (isLower, isUpper, toLower)
import Data.List (intercalate)
import Data.List.Split (keepDelimsL, split, whenElt)

options :: Options
options = defaultOptions {
  fieldLabelModifier = (\(c:cs) -> toLower c : cs) . dropWhile isLower,
  constructorTagModifier = map toLower,
  omitNothingFields = True
}

kebabCase :: String -> String
kebabCase = modifyCase "-"

snakeCase :: String -> String
snakeCase = modifyCase "_"

modifyCase :: String -> String -> String
modifyCase cs = intercalate cs . map (map toLower) . dropWhile null . split (keepDelimsL $ whenElt isUpper)
