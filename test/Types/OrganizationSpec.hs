module Types.OrganizationSpec where

import Data.Aeson (decode, encode)
import Data.Aeson.QQ
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

    it "should reject an invalid json" $
      decode "{}" `shouldBe` (Nothing :: Maybe Organization)

  describe "Organization ToJSON" $
    it "should encode into a json" $
      decode (encode organization) `shouldBe` Just json
