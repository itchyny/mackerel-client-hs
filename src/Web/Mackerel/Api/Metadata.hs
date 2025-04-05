-- | Metadata API.
module Web.Mackerel.Api.Metadata (getMetadata, putMetadata, deleteMetadata, listMetadata) where

import Data.Aeson (Value)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Metadata

getMetadata :: Client -> String -> String -> IO (Either ApiError Value)
getMetadata client hostId namespace
  = request client GET ("/api/v0/hosts/" <> BS.pack hostId <> "/metadata/" <> BS.pack namespace) [] emptyBody (createHandler id)

putMetadata :: Client -> String -> String -> Value -> IO (Either ApiError ())
putMetadata client hostId namespace metadata
  = request client PUT ("/api/v0/hosts/" <> BS.pack hostId <> "/metadata/" <> BS.pack namespace) [] (Just metadata) (createHandler ((\_ -> ()) :: Value -> ()))

deleteMetadata :: Client -> String -> String -> IO (Either ApiError ())
deleteMetadata client hostId namespace
  = request client DELETE ("/api/v0/hosts/" <> BS.pack hostId <> "/metadata/" <> BS.pack namespace) [] emptyBody (createHandler ((\_ -> ()) :: Value -> ()))

data ListMetadataResponse = ListMetadataResponse { responseMetadata :: [Metadata] }
$(deriveJSON options ''ListMetadataResponse)

listMetadata :: Client -> String -> IO (Either ApiError [Metadata])
listMetadata client hostId
  = request client GET ("/api/v0/hosts/" <> BS.pack hostId <> "/metadata") [] emptyBody (createHandler responseMetadata)
