{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.ServiceSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Service

spec :: Spec
spec = do

  let service = Service {
        serviceName = "FooService",
        serviceMemo = "service memo",
        serviceRoles = [ "role0", "role1", "role2" ]
      }

  let json = [aesonQQ|
    {
      "name": "FooService",
      "memo": "service memo",
      "roles": [
        "role0",
        "role1",
        "role2"
      ]
    }
  |]

  describe "Service FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just service

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Service)
      let (Object hm) = json
      forM_ ["name", "memo", "roles"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Service)

  describe "Service ToJSON" $
    it "should encode into a json" $
      decode (encode service) `shouldBe` Just json
