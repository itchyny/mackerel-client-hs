module Types.ChannelSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.KeyMap qualified as HM
import Data.Aeson.QQ
import Test.Hspec

import Web.Mackerel.Types.Channel

spec :: Spec
spec = do

  let channel1 = Channel {
        channelId = ChannelId "abcde1",
        channelName = "Example Channel 1",
        channelType = "slack"
      }

  let json1 = [aesonQQ|
    {
      "id": "abcde1",
      "name": "Example Channel 1",
      "type": "slack"
    }
  |]

  let channel2 = Channel {
        channelId = ChannelId "abcde2",
        channelName = "Example Channel 2",
        channelType = "email"
      }

  let json2 = [aesonQQ|
    {
      "id": "abcde2",
      "name": "Example Channel 2",
      "type": "email"
    }
  |]

  describe "Channel FromJSON" $ do
    it "should parse a json" $
      forM_ [(channel1, json1), (channel2, json2)] \(channel, json) ->
        decode (encode json) `shouldBe` Just channel

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Channel)
      let (Object hm) = json1
      forM_ ["id", "name", "type"] \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Channel)

  describe "Channel ToJSON" $
    it "should encode into a json" $
      forM_ [(channel1, json1), (channel2, json2)] \(channel, json) ->
        decode (encode channel) `shouldBe` Just json
