-- | User API.
module Web.Mackerel.Api.User (listUsers, deleteUser) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.User

data ListUsersResponse = ListUsersResponse { responseUsers :: [User] }
$(deriveJSON options ''ListUsersResponse)

listUsers :: Client -> IO (Either ApiError [User])
listUsers client
  = request client GET "/api/v0/users" [] emptyBody (createHandler responseUsers)

deleteUser :: Client -> UserId -> IO (Either ApiError User)
deleteUser client (UserId userId')
  = request client DELETE ("/api/v0/users/" <> BS.pack userId') [] emptyBody (createHandler id)
