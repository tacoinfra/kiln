{-# LANGUAGE OverloadedStrings #-}

module SnapshotMetadata
  ( testSnapshotMetadata
  ) where

import Data.Aeson
import Data.Time
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Backend.Config (AppConfig (..))
import Backend.Snapshot (findLatestSnapshot)
import Common.Schema
import Tezos.Types

testSnapshotMetadata :: TestTree
testSnapshotMetadata = testGroup "Snapshot metadata"
  [ testFindLatestRollingSnapshot
  ]

testFindLatestRollingSnapshot :: TestTree
testFindLatestRollingSnapshot =
  testCase "Find the latest rolling mainnet snapshot in xtz-shots metadata" $ do
    rawMetadata <- LBS.readFile "test/resources/metadata.json"
    let metadata = either (error "Can't parse metadata") unSnapshotMetadataList $ eitherDecode rawMetadata
    actual <- findLatestSnapshot dummyAppConfig metadata
    let
      parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
      expected = SnapshotMetadata
        { _snapshotMetadata_blockHeight = 3505126
        , _snapshotMetadata_blockHash = BlockHash "BL8gHjhuS19unbYxMjtD83wMxgUWZz39oCSPYAKg2YJRhKHpfHy"
        , _snapshotMetadata_blockTimestamp = parseTime "2023-05-10T05:35:44"
        , _snapshotMetadata_url = "https://mainnet-v16-shots.nyc3.digitaloceanspaces.com/mainnet-3505126.rolling"
        , _snapshotMetadata_chainName = "mainnet"
        , _snapshotMetadata_historyMode = SnapshotHistoryMode_Rolling
        , _snapshotMetadata_artifactType = SnapshotArtifactType_TezosSnapshot
        }
    actual @?= expected

dummyAppConfig :: AppConfig
dummyAppConfig = AppConfig
  { _appConfig_emailFromAddress = Nothing
  , _appConfig_kilnNodeRpcPort = 0
  , _appConfig_kilnNodeNetPort = 0
  , _appConfig_kilnDataDir = ""
  , _appConfig_kilnNodeConfig = dummyNodeConfigFile
  , _appConfig_chainId = "NetXdQprcVkpaWU" -- mainnet
  , _appConfig_kilnNodeCustomArgs = Nothing
  , _appConfig_kilnBakerCustomArgs = Nothing
  , _appConfig_binaryPaths = Nothing
  , _appConfig_tezosNodeEnvVar = Nothing
  , _appConfig_processRestartMaxDelay = 0
  }
  where
    dummyNodeConfigFile = Left Null
