-- | Channel API.
module Web.Mackerel.Api.Channel (listChannels) where

import Data.Aeson.TH (deriveJSON)
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Internal.TH
import Web.Mackerel.Types.Channel

data ListChannelsResponse = ListChannelsResponse { responseChannels :: [Channel] }
$(deriveJSON options ''ListChannelsResponse)

listChannels :: Client -> IO (Either ApiError [Channel])
listChannels client
  = request client GET "/api/v0/channels" [] emptyBody (createHandler responseChannels)
