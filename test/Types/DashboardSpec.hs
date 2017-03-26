{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.DashboardSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Dashboard

spec :: Spec
spec = do

  let dashboard = Dashboard {
        dashboardId = DashboardId "abcde",
        dashboardTitle = "This is a dashboard",
        dashboardBodyMarkdown = "# Example\n[example](https://example.com)",
        dashboardUrlPath = "example"
      }

  let json = [aesonQQ|
    {
      "id": "abcde",
      "title": "This is a dashboard",
      "bodyMarkdown": "# Example\n[example](https://example.com)",
      "urlPath": "example"
    }
  |]

  let dashboardCreate = DashboardCreate {
        dashboardCreateTitle = "This is a dashboard",
        dashboardCreateBodyMarkdown = "# Example\n[example](https://example.com)",
        dashboardCreateUrlPath = "example"
      }

  let jsonCreate = [aesonQQ|
    {
      "title": "This is a dashboard",
      "bodyMarkdown": "# Example\n[example](https://example.com)",
      "urlPath": "example"
    }
  |]

  describe "Dashboard FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just dashboard

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Dashboard)
      let (Object hm) = json
      forM_ ["id", "title", "bodyMarkdown", "urlPath"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Dashboard)

  describe "Dashboard ToJSON" $
    it "should encode into a json" $
      decode (encode dashboard) `shouldBe` Just json

  describe "DashboardCreate FromJSON" $
    it "should parse a json" $
      decode (encode jsonCreate) `shouldBe` Just dashboardCreate

  describe "DashboardCreate ToJSON" $
    it "should encode into a json" $
      decode (encode dashboardCreate) `shouldBe` Just jsonCreate
