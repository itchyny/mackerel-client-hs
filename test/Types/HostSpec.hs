{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types.HostSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode, Value(..))
import Data.Aeson.QQ
import Data.Default
import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Web.Mackerel.Types.Host

spec :: Spec
spec = do

  let host1 = Host {
        hostId = HostId "abcede",
        hostName = "foohost.example",
        hostDisplayName = Nothing,
        hostStatus = HostStatusWorking,
        hostMemo = "",
        hostRoles = HM.fromList [],
        hostIsRetired = False,
        hostCreatedAt = 1483196400,
        hostMeta = def,
        hostInterfaces = []
      }

  let json1 = [aesonQQ|
    {
      "id": "abcede",
      "name": "foohost.example",
      "status": "working",
      "memo": "",
      "roles": {},
      "isRetired": false,
      "createdAt": 1483196400,
      "meta": {},
      "interfaces": []
    }
  |]

  let host2 = Host {
        hostId = HostId "abcede",
        hostName = "foohost.example",
        hostDisplayName = Just "Host Foo",
        hostStatus = HostStatusWorking,
        hostMemo = "host memo",
        hostRoles = HM.fromList [("foo", ["bar", "baz"]), ("qux", ["quux"])],
        hostIsRetired = False,
        hostCreatedAt = 1483196400,
        hostMeta = def {
          metaAgentName = Just "mackerel-agent/0.37.0 (Revision 3c11c2c)",
          metaAgentRevision = Just "3c11c2c",
          metaAgentVersion = Just "0.37.0",
          metaBlockDevice = Just $ HM.fromList [
            ("xvda", HM.fromList [
              ("removable", "0"),
              ("size", "251658240")
            ])
          ],
          metaCpu = Just [
            HostMetaCpu {
              metaCpuCacheSize = Just "25600 KB",
              metaCpuCoreId = Just "0",
              metaCpuCores = Just "1",
              metaCpuFamily = Just "6",
              metaCpuMhz = Just "2500.064",
              metaCpuModel = Just "62",
              metaCpuModelName = Just "Intel(R) Xeon(R) CPU E5-2670 v2 @ 2.50GHz",
              metaCpuPhysicalId = Just "0",
              metaCpuStepping = Just "4",
              metaCpuVendorId = Just "GenuineIntel"
            }
          ],
          metaFilesystem = Just $ HM.fromList [
            ("/dev/xvda", HM.fromList [
              ("kb_available", Number $ read "2526784"),
              ("kb_size", Number $ read "8125880"),
              ("kb_used", Number $ read "5163284"),
              ("mount", "/"),
              ("percent_used", "68%")
            ])
          ],
          metaKernel = Just $ HM.fromList [
            ("machine", "x86_64"),
            ("name", "Linux"),
            ("os", "GNU/Linux"),
            ("platform_name", "Debian"),
            ("platform_version", "8.3"),
            ("release", "3.16.0-4-amd64"),
            ("version", "#1 SMP Debian 3.16.7-ckt20-1+deb8u3 (2016-01-17)")
          ],
          metaMemory = Just $ HM.fromList [
            ("active", "77456kB"),
            ("buffers", "11720kB"),
            ("cached", "83456kB"),
            ("free", "3685992kB"),
            ("inactive", "59368kB"),
            ("total", "3865572kB")
          ],
          metaCloud = Just HostMetaCloud {
            metaCloudProvider = "ec2",
            metaCloudMetadata = HM.fromList [
              ("ami-id", "ami-000"),
              ("hostname", "ip-0-0-0-0"),
              ("instance-id", "i-00000"),
              ("instance-type", "m3.medium")
            ]
          }
        },
        hostInterfaces = [
          HostInterface {
            hostInterfaceName = "en0",
            hostInterfaceMacAddress = Just "ff:ff:ff:ff:ff:ff",
            hostInterfaceIpv4Addresses = Just ["192.168.100.2"],
            hostInterfaceIpv6Addresses = Just ["2001:268:c00f:8a08::"]
          }
        ]
      }

  let json2 = [aesonQQ|
    {
      "id": "abcede",
      "name": "foohost.example",
      "displayName": "Host Foo",
      "status": "working",
      "memo": "host memo",
      "roles": {
        "foo": ["bar", "baz"],
        "qux": ["quux"]
      },
      "isRetired": false,
      "createdAt": 1483196400,
      "meta": {
        "agent-name": "mackerel-agent/0.37.0 (Revision 3c11c2c)",
        "agent-revision": "3c11c2c",
        "agent-version": "0.37.0",
        "block_device": {
          "xvda": {
            "removable": "0",
            "size": "251658240"
          }
        },
        "cpu": [
          {
            "cache_size": "25600 KB",
            "core_id": "0",
            "cores": "1",
            "family": "6",
            "mhz": "2500.064",
            "model": "62",
            "model_name": "Intel(R) Xeon(R) CPU E5-2670 v2 @ 2.50GHz",
            "physical_id": "0",
            "stepping": "4",
            "vendor_id": "GenuineIntel"
          }
        ],
        "filesystem": {
          "/dev/xvda": {
            "kb_available": 2526784,
            "kb_size": 8125880,
            "kb_used": 5163284,
            "mount": "/",
            "percent_used": "68%"
          }
        },
        "kernel": {
          "machine": "x86_64",
          "name": "Linux",
          "os": "GNU/Linux",
          "platform_name": "Debian",
          "platform_version": "8.3",
          "release": "3.16.0-4-amd64",
          "version": "#1 SMP Debian 3.16.7-ckt20-1+deb8u3 (2016-01-17)"
        },
        "memory": {
          "active": "77456kB",
          "buffers": "11720kB",
          "cached": "83456kB",
          "free": "3685992kB",
          "inactive": "59368kB",
          "total": "3865572kB"
        },
        "cloud": {
          "provider": "ec2",
          "metadata": {
            "ami-id": "ami-000",
            "hostname": "ip-0-0-0-0",
            "instance-id": "i-00000",
            "instance-type": "m3.medium"
          }
        }
      },
      "interfaces": [
        {
          "name": "en0",
          "macAddress": "ff:ff:ff:ff:ff:ff",
          "ipv4Addresses": ["192.168.100.2"],
          "ipv6Addresses": ["2001:268:c00f:8a08::"]
        }
      ]
    }
  |]

  describe "Host FromJSON" $ do
    it "should parse a json" $
      forM_ [(json1, host1), (json2, host2)] $ \(json, host) ->
        decode (encode json) `shouldBe` Just host

    it "should reject an invalid json" $ do
      decode "{}" `shouldBe` (Nothing :: Maybe Host)
      let (Object hm) = json1
      forM_ ["id", "name", "status", "memo", "roles", "isRetired", "createdAt", "meta", "interfaces"] $ \key ->
        decode (encode (Object (HM.delete key hm))) `shouldBe` (Nothing :: Maybe Host)
      forM_ ["status", "roles", "isRetired", "createdAt", "meta", "interfaces"] $ \key ->
        decode (encode (Object (HM.insert key "invalid" hm))) `shouldBe` (Nothing :: Maybe Host)

  describe "Host ToJSON" $
    it "should encode into a json" $
      forM_ [(json1, host1), (json2, host2)] $ \(json, host) ->
        decode (encode host) `shouldBe` Just json

  describe "HostStatus Show" $
    it "should show properly" $ do
      show HostStatusWorking `shouldBe` "working"
      show HostStatusMaintenance `shouldBe` "maintenance"
      show [HostStatusWorking, HostStatusStandby, HostStatusMaintenance, HostStatusPoweroff] `shouldBe` "[working,standby,maintenance,poweroff]"

  describe "HostStatus Read" $
    it "should read properly" $ do
      read "working" `shouldBe` HostStatusWorking
      read "maintenance" `shouldBe` HostStatusMaintenance
      read "[working,standby,maintenance,poweroff]" `shouldBe` [HostStatusWorking, HostStatusStandby, HostStatusMaintenance, HostStatusPoweroff]
