{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.InvitationSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Invitation

spec :: Spec
spec = do

  let invitation' = Invitation {
        invitationEmail = "foobar@example.com",
        invitationAuthority = InvitationAuthorityManager
      }

  let json = [aesonQQ|
    {
      "email": "foobar@example.com",
      "authority": "manager"
    }
  |]

  describe "Invitation FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just invitation'

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Invitation)
      let (Object hm) = json
      forM_ ["email", "authority"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Invitation)

  describe "invitation ToJSON" $
    it "should encode into a json" $
      decode (encode invitation') `shouldBe` Just json
