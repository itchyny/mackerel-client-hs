{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.UserSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Authority
import Web.Mackerel.Types.User

spec :: Spec
spec = do

  let user = User {
        userId = UserId "abcde",
        userScreenName = "Example Mackerel",
        userEmail = "mackerel@example.com",
        userAuthority = AuthorityCollaborator
      }

  let json = [aesonQQ|
    {
      "id": "abcde",
      "screenName": "Example Mackerel",
      "email": "mackerel@example.com",
      "authority": "collaborator"
    }
  |]

  describe "User FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just user

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe User)
      let (Object hm) = json
      forM_ ["id", "screenName", "email"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe User)

  describe "User ToJSON" $
    it "should encode into a json" $
      decode (encode user) `shouldBe` Just json
