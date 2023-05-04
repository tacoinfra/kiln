{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Config.Node
  ( computeChainIdFromConfigFile
  , createNodeConfigByUrl
  , fetchChainIdByUrl
  ) where

import Control.Exception.Safe (throwString)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLoggerIO, logDebug, logError)
import Data.Aeson (Object, Value (..), eitherDecode)
import Data.Aeson.Lens
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators (whenLeft)
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Validation (toEither)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import Text.URI (URI, renderStr)
import UnliftIO.Exception (bracket_)
import UnliftIO.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import UnliftIO.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Backend.Config (BinaryPaths (..), validateNodeConfigFile)
import Backend.Http (doRequestLBSThrows)
import Backend.Process.Node (nixNodePath)
import Backend.Workers.TezosClient (computeChainId)
import Tezos.Common.Base58Check (IsBase58Hash)
import Tezos.Types

import ExtraPrelude

-- | Given the network config url (e.g http://testnets.xyz/mumbainet)
-- computes the chain id of this network.
fetchChainIdByUrl
  :: ( MonadIO m
     , MonadThrow m
     , MonadLoggerIO m
    )
  => Http.Manager
  -> URI
  -> Maybe BinaryPaths
  -> m ChainId
fetchChainIdByUrl httpMgr uri mbCustomPaths = do
  let uriStr = renderStr uri
  resp <- doRequestLBSThrows httpMgr uriStr
  let body   = Http.getResponseBody resp
      status = Http.getResponseStatusCode resp
  $(logDebug) $ T.pack uriStr <> " responded with status " <> tshow status
  when (status /= 200) $
    throwString $ concat
      [ "Expected the response status code of "
      , uriStr
      , " to be 200, but got "
      , show status
      ]
  case eitherDecode @Value body of
    Left err ->
      throwString $ "Failed to decode the response: " <> err
    Right json -> do
      let mbGenesis = json ^? key "genesis" . _Object
      genesis   <- maybe (throwString "Key 'genesis' isn't present in the network config") pure mbGenesis
      blkHash   <- fmap BlockHash . fromRawHash =<< getRawGenesisValue genesis "block"
      protoHash <- fromRawHash =<< getRawGenesisValue genesis "protocol"
      eiChainId <- computeChainId mbCustomPaths protoHash blkHash
      either (throwString . T.unpack) pure eiChainId

-- | Given the network config url (e.g http://testnets.xyz/mumbainet)
-- creates the node config file by executing 'octez-node config init' command
-- with temporary data directory, and then loads this config to Kiln.
createNodeConfigByUrl
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadThrow m
     )
  => FilePath
  -> URI
  -> Maybe BinaryPaths
  -> m Value
createNodeConfigByUrl kilnDataDir uri mbCustomPaths = do
  let tmpDir = kilnDataDir </> "config_tmp"
  bracket_
    (createDirectoryIfMissing True tmpDir)
    (removeDirectoryRecursive tmpDir) $ do
      initNodeConfigByUrl uri mbCustomPaths tmpDir
      rawConfig <- liftIO $ LBS.readFile $ tmpDir </> "config.json"
      case eitherDecode @Value rawConfig of
        Left err -> throwString $ "Failed to decode the node config: " <> err
        Right v -> pure v

-- | Executes "octez node config init --data-dir @dataDir@  --network @uri@"
-- command. Throws an exception if this command finished with non-zero exit code.
initNodeConfigByUrl
  :: ( MonadUnliftIO m
     , MonadThrow m
     , MonadLoggerIO m
     )
  => URI
  -> Maybe BinaryPaths
  -> FilePath
  -> m ()
initNodeConfigByUrl uri mbCustomPaths dataDir = do
  let nodePath = maybe nixNodePath _binaryPaths_nodePath mbCustomPaths
      args =
        [ "config"
        , "init"
        , "--data-dir"
        , dataDir
        , "--network"
        , renderStr uri
        ]
  (exitCode, stdout, stderr) <- readProcessWithExitCode nodePath args ""
  let cmdText = T.pack $ unwords (nodePath : args)
  case exitCode of
    ExitSuccess -> $(logDebug) $
      cmdText <> " command finished successfully. stdout: " <> T.pack stdout
    ExitFailure ec -> do
      $(logError) $ cmdText <> " failed with exit code " <> tshow ec <> ". stderr: " <> T.pack stderr
      throwString $ "Failed to init node config using url " <> renderStr uri

-- | Given the json-encoded node config file, computes the chain id from it.
computeChainIdFromConfigFile
  :: ( MonadIO m
     , MonadThrow m
     , MonadLoggerIO m
     )
  => Maybe BinaryPaths
  -> Value
  -> m ChainId
computeChainIdFromConfigFile mbCustomPaths json = do
  whenLeft (toEither . first toList $ validateNodeConfigFile json) $ \errs ->
    throwString $ T.unpack $ T.intercalate ":" errs
  let genesis = json ^. key "network" . key "genesis" . _Object
  blkHash   <- fmap BlockHash . fromRawHash =<< getRawGenesisValue genesis "block"
  protoHash <- fromRawHash =<< getRawGenesisValue genesis "protocol"
  eiChainId <- computeChainId mbCustomPaths protoHash blkHash
  either (throwString . T.unpack) pure eiChainId

fromRawHash :: (IsBase58Hash a, MonadThrow m) => Text -> m (HashedValue a)
fromRawHash raw = either (throwString . show) pure (fromBase58 $ encodeUtf8 raw)

getRawGenesisValue :: MonadThrow m => Object -> Text -> m Text
getRawGenesisValue (Object -> genesis) keyName =
  let
    mbRawValue = genesis ^? key keyName . _String
    errMsg = concat
      [ "Key 'genesis."
      , T.unpack keyName
      , "' isn't present in the network config"
      ]
  in maybe (throwString errMsg) pure mbRawValue
