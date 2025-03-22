{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.MetadataSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import qualified Data.Aeson.KeyMap as HM
import Test.Hspec

import Web.Mackerel.Types.Metadata

spec :: Spec
spec = do

  let metadata' = Metadata {
        metadataNamespace = "foo-bar-namespace"
      }

  let json = [aesonQQ|
    {
      "namespace": "foo-bar-namespace"
    }
  |]

  describe "Metadata FromJSON" $ do
    it "should parse a json" $
      decode (encode json) `shouldBe` Just metadata'

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Metadata)
      let (Object hm) = json
      forM_ ["namespace"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Metadata)

  describe "Metadata ToJSON" $
    it "should encode into a json" $
      decode (encode metadata') `shouldBe` Just json
