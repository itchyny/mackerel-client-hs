-- | Role API.
module Web.Mackerel.Api.Role
  ( listRoles
  , createRole
  , deleteRole
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Role

data ListRolesResponse = ListRolesResponse { responseRoles :: [Role] }
$(deriveJSON options ''ListRolesResponse)

listRoles :: Client -> String -> IO (Either ApiError [Role])
listRoles client serviceName'
  = request client GET ("/api/v0/services/" <> BS.pack serviceName' <> "/roles") [] emptyBody (createHandler responseRoles)

createRole :: Client -> String -> Role -> IO (Either ApiError Role)
createRole client serviceName' role
  = request client POST ("/api/v0/services/" <> BS.pack serviceName' <> "/roles") [] (Just role) (createHandler id)

deleteRole :: Client -> String -> String -> IO (Either ApiError Role)
deleteRole client serviceName' name
  = request client DELETE ("/api/v0/services/" <> BS.pack serviceName' <> "/roles/" <> BS.pack name) [] emptyBody (createHandler id)
