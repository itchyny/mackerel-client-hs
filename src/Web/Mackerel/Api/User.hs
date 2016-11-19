{-# LANGUAGE TemplateHaskell #-}
-- | User API.
module Web.Mackerel.Api.User (listUsers, deleteUser) where

import Data.Aeson.TH (deriveJSON)
import Network.Http.Client

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.User

data ListUsersResponse = ListUsersResponse { responseUsers :: [User] }
$(deriveJSON options ''ListUsersResponse)

listUsers :: Client -> IO (Either ApiError [User])
listUsers client
  = request client GET "/api/v0/users" [] emptyBody (createHandler responseUsers)

deleteUser :: Client -> String -> IO (Either ApiError User)
deleteUser client userId'
  = request client DELETE ("/api/v0/users/" ++ userId') [] emptyBody (createHandler id)