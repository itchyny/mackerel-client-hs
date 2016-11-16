{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.OrganizationSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Organization

spec :: Spec
spec = do

  let organization = Organization {
        organizationName = "FooOrganization"
      }

  let json = [aesonQQ|
    {
      "name": "FooOrganization"
    }
  |]

  describe "Organization FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just organization

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Organization)

  describe "Organization ToJSON" $
    it "should encode into a json" $
      decode (encode organization) `shouldBe` Just json
