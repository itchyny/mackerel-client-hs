-- | GraphAnnotation API.
module Web.Mackerel.Api.GraphAnnotation
  ( listGraphAnnotations
  , createGraphAnnotation
  , updateGraphAnnotation
  , deleteGraphAnnotation
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.GraphAnnotation

data ListGraphAnnotationsResponse = ListGraphAnnotationsResponse { responseGraphAnnotations :: [GraphAnnotation] }
$(deriveJSON options ''ListGraphAnnotationsResponse)

listGraphAnnotations :: Client -> String -> Integer -> Integer -> IO (Either ApiError [GraphAnnotation])
listGraphAnnotations client service from to = do
  let query = [ ("service", Just $ BS.pack service), ("from", Just $ BS.pack $ show from), ("to", Just $ BS.pack $ show to) ]
  request client GET "/api/v0/graph-annotations" query emptyBody (createHandler responseGraphAnnotations)

createGraphAnnotation :: Client -> GraphAnnotation -> IO (Either ApiError GraphAnnotation)
createGraphAnnotation client graphAnnotation
  = request client POST "/api/v0/graph-annotations" [] (Just graphAnnotation) (createHandler id)

updateGraphAnnotation :: Client -> GraphAnnotation -> IO (Either ApiError GraphAnnotation)
updateGraphAnnotation client graphAnnotation = do
  let Just (GraphAnnotationId graphAnnotationId') = graphAnnotationId graphAnnotation
  request client PUT ("/api/v0/graph-annotations/" <> BS.pack graphAnnotationId') [] (Just graphAnnotation) (createHandler id)

deleteGraphAnnotation :: Client -> GraphAnnotationId -> IO (Either ApiError GraphAnnotation)
deleteGraphAnnotation client (GraphAnnotationId graphAnnotationId')
  = request client DELETE ("/api/v0/graph-annotations/" <> BS.pack graphAnnotationId') [] emptyBody (createHandler id)
