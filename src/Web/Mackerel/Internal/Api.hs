{-# LANGUAGE OverloadedStrings #-}
-- | Internal helpers for APIs.
module Web.Mackerel.Internal.Api (request, createHandler, ApiError(..)) where

import Control.Monad ((>=>), guard)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as BSChar8
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup ((<>))
import Network.Http.Client
import Network.HTTP.Types (Query, renderQuery)
import Network.URI (uriAuthority, uriScheme, URIAuth(..))
import System.IO.Streams (InputStream, OutputStream)

import Web.Mackerel.Client

type ResponseHandler a = Response -> InputStream BS.ByteString -> IO (Either ApiError a)

data ApiError = ApiError { statusCode :: Int, errorMessage :: String } deriving (Eq, Show)

-- | Request to Mackerel.
request :: Client -> Method -> String -> Query -> (OutputStream Builder -> IO ()) -> ResponseHandler a -> IO (Either ApiError a)
request client method path query body handler = do
  let uri = apiBase client
  let uriAuth = fromJust $ uriAuthority uri
  let port | not $ null $ drop 1 $ uriPort uriAuth = read $ drop 1 $ uriPort uriAuth
           | uriScheme uri == "https:" = 443
           | otherwise = 80
  let hostname = BSChar8.pack (uriRegName uriAuth)
  ctx <- if port == 443
            then baselineContextSSL >>= \sslCtx -> openConnectionSSL sslCtx hostname port
            else openConnection hostname port
  let req = buildRequest1 $ do
        http method (BSChar8.pack path <> renderQuery True query)
        setHeader "X-Api-Key" (BSChar8.pack $ apiKey client)
        setHeader "User-Agent" (BSChar8.pack $ userAgent client)
        setHeader "Content-Type" "application/json"
        mapM_ (uncurry setAuthorizationBasic) $ do
          let info = uriUserInfo uriAuth
          guard $ not (null info) && last info == '@'
          let (user, pass') = break (==':') (init info)
          Just (BSChar8.pack user, BSChar8.pack (tail pass'))
  sendRequest ctx req body
  res <- receiveResponse ctx handler
  closeConnection ctx
  return res

-- | Create an api response handler.
createHandler :: FromJSON a => (a -> b) -> ResponseHandler b
createHandler extractor response input
  | getStatusCode response == 200 = Right . extractor <$> jsonHandler response input
  | otherwise = Left . getApiError <$> jsonHandler response input
  where getApiError json = ApiError {
          statusCode = getStatusCode response,
          errorMessage = fromMaybe "" $ parseMaybe (((.: "error") >=> (.: "message")) <> (.: "error")) json
        }
