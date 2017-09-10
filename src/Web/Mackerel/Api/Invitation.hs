{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Invitation API.
module Web.Mackerel.Api.Invitation (createInvitation, revokeInvitation) where

import Data.Aeson (Value)
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Types (StdMethod(..))

import Web.Mackerel.Client
import Web.Mackerel.Internal.Api
import Web.Mackerel.Types.Invitation

createInvitation :: Client -> Invitation -> IO (Either ApiError Invitation)
createInvitation client invitation
  = request client POST "/api/v0/invitations" [] (Just invitation) (createHandler id)

revokeInvitation :: Client -> String -> IO (Either ApiError Value)
revokeInvitation client email
  = request client POST "/api/v0/invitations/revoke" [] (Just (HM.singleton "email" email :: HM.HashMap String String)) (createHandler id)
