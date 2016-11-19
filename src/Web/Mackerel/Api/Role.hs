{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Role API.
module Web.Mackerel.Api.Role (listRoles) where

import Data.Aeson.TH (deriveJSON)
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Role

data ListRolesResponse = ListRolesResponse { responseRoles :: [Role] }
$(deriveJSON options ''ListRolesResponse)

listRoles :: Client -> String -> IO (Either ApiError [Role])
listRoles client serviceName'
  = request client GET ("/api/v0/services/" ++ serviceName' ++ "/roles") [] "" (createHandler responseRoles)
