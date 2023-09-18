{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module SnapshotMetadata
  ( testSnapshotMetadata
  ) where

import Control.Exception (SomeException, handle)
import Data.Aeson
import Data.List (isInfixOf)
import Data.Time hiding (parseTime)
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Backend.Config (AppConfig (..))
import Backend.Snapshot (findLatestCompatibleSnapshot)
import Common.Schema
import Tezos.Types

testSnapshotMetadata :: TestTree
testSnapshotMetadata = testGroup "Snapshot metadata"
  [ testFindLatestSnapshotWithExactVersion
  , testFindLatestSnapshotWithSameMajorVersion
  , testThrowsErrorWhenCouldntFindCompatibleSnapshot
  , testCanParseUnexpectedOctezVersions
  ]

testFindLatestSnapshotWithExactVersion :: TestTree
testFindLatestSnapshotWithExactVersion = baseMetadataTest
  "Prefers the snapshot with exact same Octez version"
  "test/resources/metadata_1.json"
  (MajorMinorVersion 17 3 Nothing Release) $
  SnapshotMetadata
    { _snapshotMetadata_blockHeight = 100
    , _snapshotMetadata_blockHash = BlockHash "BLagBK76j8WwzqZbaFnZo1mgziHcoZoDXqddSzDPd2LL8qPZdcb"
    , _snapshotMetadata_blockTimestamp = parseTime "2023-09-06T23:04:55"
    , _snapshotMetadata_url = "https://mainnet-v17-shots.nyc3.digitaloceanspaces.com/mainnet-4185583.rolling"
    , _snapshotMetadata_chainName = "mainnet"
    , _snapshotMetadata_historyMode = SnapshotHistoryMode_Rolling
    , _snapshotMetadata_artifactType = SnapshotArtifactType_TezosSnapshot
    , _snapshotMetadata_tezosVersion = MajorMinorVersion 17 3 Nothing Release
    , _snapshotMetadata_snapshotVersion = Just 5
    }

testFindLatestSnapshotWithSameMajorVersion :: TestTree
testFindLatestSnapshotWithSameMajorVersion = baseMetadataTest
  "Picks the snapshot with the same major version when couldn't find exact same version"
  "test/resources/metadata_2.json"
  (MajorMinorVersion 17 3 Nothing Release) $
  SnapshotMetadata
    { _snapshotMetadata_blockHeight = 50
      , _snapshotMetadata_blockHash = BlockHash "BLagBK76j8WwzqZbaFnZo1mgziHcoZoDXqddSzDPd2LL8qPZdcb"
      , _snapshotMetadata_blockTimestamp = parseTime "2023-09-06T23:04:55"
      , _snapshotMetadata_url = "https://mainnet-v17-shots.nyc3.digitaloceanspaces.com/mainnet-4185583.rolling"
      , _snapshotMetadata_chainName = "mainnet"
      , _snapshotMetadata_historyMode = SnapshotHistoryMode_Rolling
      , _snapshotMetadata_artifactType = SnapshotArtifactType_TezosSnapshot
      , _snapshotMetadata_tezosVersion = MajorMinorVersion 17 2 Nothing Release
      , _snapshotMetadata_snapshotVersion = Just 5
    }

testThrowsErrorWhenCouldntFindCompatibleSnapshot :: TestTree
testThrowsErrorWhenCouldntFindCompatibleSnapshot =
  testCase "Throws error when couldn't find compatible snapshot" $ do
    rawMetadata <- LBS.readFile "test/resources/metadata_3.json"
    let metadata = either (error "Can't parse metadata") unSnapshotMetadataList $ eitherDecode rawMetadata
        kilnNodeVersion = MajorMinorVersion 17 3 Nothing Release
    hasError <- handle handler $ findLatestCompatibleSnapshot dummyAppConfig kilnNodeVersion metadata >> pure False
    if hasError
    then pure ()
    else assertFailure "Didn't catch the expected error"
  where
    handler :: SomeException -> IO Bool
    handler e =
      let expectedError = "Couldn't find compatible snapshot in the snapshot provider metadata"
      in pure $ expectedError `isInfixOf` show e

testCanParseUnexpectedOctezVersions :: TestTree
testCanParseUnexpectedOctezVersions =
  testCase "Can parse unexpected Octez versions in snapshot metadata" $ do
    rawMetadata <- LBS.readFile "test/resources/metadata_unexpected.json"
    let eiMetadata = eitherDecode rawMetadata
        expected =
          SnapshotMetadata
            { _snapshotMetadata_blockHeight = 50
              , _snapshotMetadata_blockHash = BlockHash "BLagBK76j8WwzqZbaFnZo1mgziHcoZoDXqddSzDPd2LL8qPZdcb"
              , _snapshotMetadata_blockTimestamp = parseTime "2023-09-06T23:04:55"
              , _snapshotMetadata_url = "https://mainnet-v17-shots.nyc3.digitaloceanspaces.com/mainnet-4185583.rolling"
              , _snapshotMetadata_chainName = "mainnet"
              , _snapshotMetadata_historyMode = SnapshotHistoryMode_Rolling
              , _snapshotMetadata_artifactType = SnapshotArtifactType_TezosSnapshot
              , _snapshotMetadata_tezosVersion = MajorMinorVersion 17 0 Nothing Unknown
              , _snapshotMetadata_snapshotVersion = Just 5
            }
    case eiMetadata of
      Left err -> assertFailure $ "Expected to parse the metadata, but failed with: " <> show err
      Right m -> unSnapshotMetadataList m @?= [expected]

baseMetadataTest :: TestName -> FilePath -> MajorMinorVersion -> SnapshotMetadata -> TestTree
baseMetadataTest testName filePath kilnNodeVersion expected =
  testCase testName $ do
    rawMetadata <- LBS.readFile filePath
    let metadata = either (error "Can't parse metadata") unSnapshotMetadataList $ eitherDecode rawMetadata
    actual <- findLatestCompatibleSnapshot dummyAppConfig kilnNodeVersion metadata
    actual @?= expected

parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

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
