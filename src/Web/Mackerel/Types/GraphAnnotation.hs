{-# LANGUAGE TemplateHaskell #-}
module Web.Mackerel.Types.GraphAnnotation where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import Data.Default (Default(..))
import qualified Data.Text as Text

import Web.Mackerel.Internal.TH

data GraphAnnotationId = GraphAnnotationId String
                       deriving (Eq, Show)

instance FromJSON GraphAnnotationId where
  parseJSON (Aeson.String graphAnnotationId') = return $ GraphAnnotationId $ Text.unpack graphAnnotationId'
  parseJSON o = typeMismatch "GraphAnnotationId" o

instance ToJSON GraphAnnotationId where
  toJSON (GraphAnnotationId graphAnnotationId') = toJSON graphAnnotationId'

data GraphAnnotation
  = GraphAnnotation {
    graphAnnotationId :: Maybe GraphAnnotationId,
    graphAnnotationTitle :: String,
    graphAnnotationDescription :: String,
    graphAnnotationFrom :: Integer,
    graphAnnotationTo :: Integer,
    graphAnnotationService :: String,
    graphAnnotationRoles :: Maybe [String]
  } deriving (Eq, Show)

instance Default GraphAnnotation where
  def = GraphAnnotation def def def def def def def

$(deriveJSON options { fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 15 } ''GraphAnnotation)
