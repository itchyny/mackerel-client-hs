-- | Organization API.
module Web.Mackerel.Api.Organization (getOrganization) where

import Network.Http.Client

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Types.Organization

getOrganization :: Client -> IO (Either ApiError Organization)
getOrganization client = request client GET "/api/v0/org" [] emptyBody (createHandler id)
