module Types.RoleSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.KeyMap qualified as HM
import Data.Aeson.QQ
import Test.Hspec

import Web.Mackerel.Types.Role

spec :: Spec
spec = do

  let role' = Role {
        roleName = "FooRole",
        roleMemo = "Role memo"
      }

  let json = [aesonQQ|
    {
      "name": "FooRole",
      "memo": "Role memo"
    }
  |]

  describe "Role FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just role'

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Role)
      let (Object hm) = json
      forM_ ["name", "memo"] \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Role)

  describe "Role ToJSON" $
    it "should encode into a json" $
      decode (encode role') `shouldBe` Just json
