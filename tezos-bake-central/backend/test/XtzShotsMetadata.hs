{-# LANGUAGE OverloadedStrings #-}

module XtzShotsMetadata
  ( testXtzShotsMetadata
  ) where

import Data.Aeson
import Data.Time
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Backend.Config (AppConfig (..))
import Backend.Snapshot (findLatestSnapshot)
import Common.Schema (XtzShotsSnapshotHistoryMode (..), XtzShotsArtifactType (..), XtzShotsMetadata (..))
import Tezos.Types

testXtzShotsMetadata :: TestTree
testXtzShotsMetadata = testGroup "xtz-shots metadata"
  [ testFindLatestRollingSnapshot
  ]

testFindLatestRollingSnapshot :: TestTree
testFindLatestRollingSnapshot =
  testCase "Find the latest rolling limanet snapshot in xtz-shots metadata" $ do
    rawMetadata <- LBS.readFile "test/resources/metadata.json"
    let metadata = either (error "Can't parse metadata") id $ eitherDecode rawMetadata
    actual <- findLatestSnapshot dummyAppConfig metadata
    let
      parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
      expected = XtzShotsMetadata
        { _xtzShotsMetadata_blockHeight = 595013
        , _xtzShotsMetadata_blockHash = BlockHash "BMSxux2SLrsdJnz2fiVZQQkzUytBWK4d7hixPSkZPr3Z534i7Ut"
        , _xtzShotsMetadata_blockTimestamp = parseTime "2023-01-30T04:19:10"
        , _xtzShotsMetadata_url = "https://limanet-v15.xtz-shots.io/limanet-595013.rolling"
        , _xtzShotsMetadata_chainName = "limanet"
        , _xtzShotsMetadata_historyMode = XtzShotsSnapshotHistoryMode_Rolling
        , _xtzShotsMetadata_artifactType = XtzShotsArtifactType_TezosSnapshot
        }
    actual @?= expected

dummyAppConfig :: AppConfig
dummyAppConfig = AppConfig
  { _appConfig_emailFromAddress = Nothing
  , _appConfig_kilnNodeRpcPort = 0
  , _appConfig_kilnNodeNetPort = 0
  , _appConfig_kilnDataDir = ""
  , _appConfig_kilnNodeConfig = dummyNodeConfigFile
  , _appConfig_chainId = "NetXizpkH94bocH" -- limanet
  , _appConfig_kilnNodeCustomArgs = Nothing
  , _appConfig_kilnBakerCustomArgs = Nothing
  , _appConfig_binaryPaths = Nothing
  , _appConfig_tezosNodeEnvVar = Nothing
  }
  where
    dummyNodeConfigFile = Left Null
