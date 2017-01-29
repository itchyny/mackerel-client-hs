{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.GraphAnnotationSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.GraphAnnotation

spec :: Spec
spec = do

  let graphAnnotation = GraphAnnotation {
        graphAnnotationId = Just $ GraphAnnotationId "abcde",
        graphAnnotationTitle = "Deploy application #123",
        graphAnnotationDescription = "Deploy link: https://example.com/123",
        graphAnnotationFrom = 1485000000,
        graphAnnotationTo = 1485000060,
        graphAnnotationService = "ExampleService",
        graphAnnotationRoles = Just [ "ExampleRole1", "ExampleRole2" ]
      }

  let json = [aesonQQ|
    {
      "id": "abcde",
      "title": "Deploy application #123",
      "description": "Deploy link: https://example.com/123",
      "from": 1485000000,
      "to": 1485000060,
      "service": "ExampleService",
      "roles": [ "ExampleRole1", "ExampleRole2" ]
    }
  |]

  describe "GraphAnnotation FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just graphAnnotation

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe GraphAnnotation)
      let (Object hm) = json
      forM_ ["title", "description", "from", "to", "service"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe GraphAnnotation)

  describe "GraphAnnotation ToJSON" $
    it "should encode into a json" $
      decode (encode graphAnnotation) `shouldBe` Just json
