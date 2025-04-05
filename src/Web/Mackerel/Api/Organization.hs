-- | Organization API.
module Web.Mackerel.Api.Organization (getOrganization) where

import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Types.Organization

getOrganization :: Client -> IO (Either ApiError Organization)
getOrganization client = request client GET "/api/v0/org" [] emptyBody (createHandler id)
