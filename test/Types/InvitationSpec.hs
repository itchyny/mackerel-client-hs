{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.InvitationSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.Aeson.KeyMap as HM
import Test.Hspec

import Web.Mackerel.Types.Authority
import Web.Mackerel.Types.Invitation

spec :: Spec
spec = do

  let invitation' = Invitation {
        invitationEmail = "foobar@example.com",
        invitationAuthority = AuthorityManager,
        invitationExpiresAt = 1492393387
      }

  let json = [aesonQQ|
    {
      "email": "foobar@example.com",
      "authority": "manager",
      "expiresAt": 1492393387
    }
  |]

  describe "Invitation FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just invitation'

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Invitation)
      let (Object hm) = json
      forM_ ["email", "authority", "expiresAt"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Invitation)

  describe "Invitation ToJSON" $
    it "should encode into a json" $
      decode (encode invitation') `shouldBe` Just json

  let invitationCreate' = InvitationCreate {
        invitationCreateEmail = "foobar@example.com",
        invitationCreateAuthority = AuthorityCollaborator
      }

  let jsonCreate = [aesonQQ|
    {
      "email": "foobar@example.com",
      "authority": "collaborator"
    }
  |]

  describe "InvitationCreate FromJSON" $ do
    it "should parse a json" $
      decode (encode jsonCreate) `shouldBe` Just invitationCreate'

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe InvitationCreate)
      let (Object hm) = jsonCreate
      forM_ ["email", "authority"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe InvitationCreate)

  describe "InvitationCreate ToJSON" $
    it "should encode into a json" $
      decode (encode invitationCreate') `shouldBe` Just jsonCreate
