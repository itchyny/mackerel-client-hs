-- | Invitation API.
module Web.Mackerel.Api.Invitation (createInvitation, revokeInvitation) where

import Data.Aeson (Value)
import Data.HashMap.Lazy qualified as HM
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Types.Invitation

createInvitation :: Client -> InvitationCreate -> IO (Either ApiError Invitation)
createInvitation client invitation
  = request client POST "/api/v0/invitations" [] (Just invitation) (createHandler id)

revokeInvitation :: Client -> String -> IO (Either ApiError ())
revokeInvitation client email
  = request client POST "/api/v0/invitations/revoke" [] (Just (HM.singleton "email" email :: HM.HashMap String String)) (createHandler ((\_ -> ()) :: Value -> ()))
