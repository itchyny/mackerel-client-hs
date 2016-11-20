{-# LANGUAGE OverloadedStrings #-}
-- | Internal helpers for APIs.
module Web.Mackerel.Internal.Api (request, createHandler, ApiError(..)) where

import Control.Monad ((>=>))
import Data.Aeson (decode, (.:), FromJSON(..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (maybe, fromMaybe)
import Data.Semigroup ((<>))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (StdMethod, renderStdMethod, Query, statusCode)

import Web.Mackerel.Client

type ResponseHandler a = Response LBS.ByteString -> Either ApiError a

data ApiError = ApiError { errorStatusCode :: Int, errorMessage :: String } deriving (Eq, Show)

-- | Request to Mackerel.
request :: Client -> StdMethod -> BS.ByteString -> Query -> LBS.ByteString -> ResponseHandler a -> IO (Either ApiError a)
request client method' path' query body handler = do
  initialRequest <- setQueryString query <$> parseRequest (apiBase client)
  handler <$> (httpLbs initialRequest {
    method = renderStdMethod method', path = path', requestBody = RequestBodyLBS body,
    requestHeaders = requestHeaders initialRequest ++ [
      ("X-Api-Key", BS.pack $ apiKey client),
      ("User-Agent", BS.pack $ userAgent client),
      ("Content-Type", "application/json")
    ]
  } =<< newManager (if secure initialRequest then tlsManagerSettings else defaultManagerSettings))

-- | Create an api response handler.
createHandler :: FromJSON a => (a -> b) -> ResponseHandler b
createHandler extractor response
  | statusCode (responseStatus response) == 200 = maybe (Left decodeError) (Right . extractor) (decode (responseBody response))
  | otherwise = maybe (Left decodeError) (Left . getApiError) (decode (responseBody response))
  where getApiError json = ApiError {
          errorStatusCode = statusCode $ responseStatus response,
          errorMessage = fromMaybe "" $ parseMaybe (((.: "error") >=> (.: "message")) <> (.: "error")) json
        }
        decodeError = ApiError {
          errorStatusCode = statusCode $ responseStatus response,
          errorMessage = show $ responseBody response
        }
