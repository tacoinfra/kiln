{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Config where

import Control.Lens (Lens', view)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Value(..))
import Data.Either (fromRight)
import Data.Validation
import Data.Word
import Network.Mail.Mime (Address)
import System.FilePath ((</>))
import Text.URI (URI)
import Data.Aeson.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Language.Haskell.TH.Quote as QQ
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Common.Config (defaultKilnNodeRpcPort)
import Common.URI (Port)
import ExtraPrelude
import Tezos.Types (toBase58Text, ChainId, ProtocolHash, tezosJsonOptions)

data AppConfig = AppConfig
  { _appConfig_emailFromAddress :: Address
  , _appConfig_kilnNodeRpcPort :: Port
  , _appConfig_kilnNodeNetPort :: Port
  , _appConfig_kilnDataDir :: FilePath
  , _appConfig_kilnNodeConfig :: NodeConfigFile
  , _appConfig_chainId :: ChainId
  , _appConfig_kilnNodeCustomArgs :: Maybe Text
  , _appConfig_kilnBakerCustomArgs :: Maybe Text
  , _appConfig_binaryPaths :: Maybe BinaryPaths
  , _appConfig_tezosNodeEnvVar :: Maybe FilePath
  }

class HasAppConfig a where
  getAppConfig :: Lens' a AppConfig

instance HasAppConfig AppConfig where
  getAppConfig = id

askAppConfig :: (HasAppConfig a, MonadReader a m) => m AppConfig
askAppConfig = asks $ view getAppConfig

kilnNodeRpcURI :: AppConfig -> URI
kilnNodeRpcURI = kilnNodeRpcURI' . _appConfig_kilnNodeRpcPort

kilnNodeRpcURI' :: Port -> URI
kilnNodeRpcURI' port = fromRight $(QQ.quoteExp Uri.uri $ "http://127.0.0.1:" <> show defaultKilnNodeRpcPort) $
  Uri.mkURI ("http://127.0.0.1:" <> tshow port)

(>>=?) :: Validation e a -> (a -> Validation e b) -> Validation e b
v >>=? f = bindValidation v f

validateNodeConfigFile :: NodeConfigFile -> Validation (NonEmpty Text) NodeConfigFile
validateNodeConfigFile = \case
  Right r -> pure $ Right r
  Left json -> Left <$> validateTrulyCustom json

validateTrulyCustom :: Value -> Validation (NonEmpty Text) Value
validateTrulyCustom json =
   validationNel (maybe (Left "network unavailable") Right (json ^? key "network" . _Object)) >>=? \(Object -> network) ->
       validationNel (maybe (Left "network.genesis unavailable") Right (network ^? key "genesis" . _Object)) >>=? \(Object -> genesis) ->
          do
            validationNel $ maybe (Left "network.genesis.timestamp unavailable") Right (genesis ^? key "timestamp")
            validationNel $ maybe (Left "network.genesis.block unavailable") Right (genesis ^? key "block")
            validationNel $ maybe (Left "network.genesis.protocol unavailable") Right (genesis ^? key "protocol")
            validationNel $ maybe (Left "network.genesis.chain_name unavailable") Right (network ^? key "chain_name")
            validationNel $ maybe (Left "network.genesis.sandboxed_chain_name unavailable") Right (network ^? key "sandboxed_chain_name")
            pure json

nodeDataDir :: AppConfig -> FilePath
nodeDataDir appConfig = _appConfig_kilnDataDir appConfig
    </> case _appConfig_kilnNodeConfig appConfig of
          Left json -> fromMaybe "tezos-node" $ getDataDir json <|> _appConfig_tezosNodeEnvVar appConfig
          Right ncf -> fromMaybe "tezos-node" (_nodeConfigFile_dataDir ncf) </> T.unpack (toBase58Text $ _appConfig_chainId appConfig)
  where
    getDataDir json = T.unpack <$> json ^? key "data-dir" . _String

tezosClientDataDir :: AppConfig -> FilePath
tezosClientDataDir appConfig = _appConfig_kilnDataDir appConfig </> "tezos-client"

type NodeConfigFile = Either Value NodeConfigFile'

defaultNodeConfigFile :: NodeConfigFile'
defaultNodeConfigFile = NodeConfigFile'
  { _nodeConfigFile_p2p = NodeConfigP2P
    { _nodeConfigP2P_expectedProofOfWork = Nothing
    , _nodeConfigP2P_bootstrapPeers = Nothing
    , _nodeConfigP2P_listenAddr = Nothing
    , _nodeConfigP2P_privateMode = Nothing
    , _nodeConfigP2P_disableMempool = Nothing
  }
  , _nodeConfigFile_dataDir = Nothing
  , _nodeConfigFile_rpc = Just NodeConfigRPC
    { _nodeConfigRPC_listenAddr = Just "127.0.0.1"
    , _nodeConfigRPC_corsOrigin = Nothing
    , _nodeConfigRPC_corsHeaders = Nothing
    -- Nothing
    , _nodeConfigRPC_crt = Nothing
    , _nodeConfigRPC_key = Nothing
    }
  , _nodeConfigFile_log = Nothing
  , _nodeConfigFile_shell = Nothing
  , _nodeConfigFile_network = Just "mainnet"
  }

data NodeConfigRPC = NodeConfigRPC
  { _nodeConfigRPC_listenAddr :: Maybe Text
  , _nodeConfigRPC_corsOrigin :: Maybe [Text]
  , _nodeConfigRPC_corsHeaders :: Maybe [Text]
  , _nodeConfigRPC_crt :: Maybe Text
  , _nodeConfigRPC_key :: Maybe Text
  }

data NodeConfigP2PLimits = NodeConfigP2PLimits
  { _nodeConfigP2PLimits_connectionTimeout :: Maybe Double
  , _nodeConfigP2PLimits_authenticationTimeout :: Maybe Double
  , _nodeConfigP2PLimits_minConnections :: Maybe Word16
  , _nodeConfigP2PLimits_expectedConnections :: Maybe Word16
  , _nodeConfigP2PLimits_maxConnections :: Maybe Word16
  , _nodeConfigP2PLimits_backlog :: Maybe Word8
  , _nodeConfigP2PLimits_maxIncomingConnections :: Maybe Word8
  , _nodeConfigP2PLimits_maxDownloadSpeed :: Maybe Int
  , _nodeConfigP2PLimits_maxUploadSpeed :: Maybe Int
  , _nodeConfigP2PLimits_swapLinger :: Maybe Double
  , _nodeConfigP2PLimits_binaryChunksSize :: Maybe Word8
  , _nodeConfigP2PLimits_readBufferSize :: Maybe Int
  , _nodeConfigP2PLimits_readQueueSize :: Maybe Int
  , _nodeConfigP2PLimits_writeQueueSize :: Maybe Int
  , _nodeConfigP2PLimits_incomingAppMessageQueueSize :: Maybe Int
  , _nodeConfigP2PLimits_incomingMessageQueueSize :: Maybe Int
  , _nodeConfigP2PLimits_outgoingMessageQueueSize :: Maybe Int
  , _nodeConfigP2PLimits_knownPointsHistorySize :: Maybe Word16
  , _nodeConfigP2PLimits_knownPeerIdsHistorySize :: Maybe Word16
  , _nodeConfigP2PLimits_maxKnownPoints :: Maybe (Int, Int)
  , _nodeConfigP2PLimits_maxKnownPeerIds :: Maybe (Int, Int)
  , _nodeConfigP2PLimits_greylistTimeout :: Maybe Int
  }

data NodeConfigP2P = NodeConfigP2P
  { _nodeConfigP2P_expectedProofOfWork :: Maybe Double
  , _nodeConfigP2P_bootstrapPeers :: Maybe [Text]
  , _nodeConfigP2P_listenAddr :: Maybe Text
  , _nodeConfigP2P_privateMode :: Maybe Bool
  , _nodeConfigP2P_disableMempool :: Maybe Bool
  }
data NodeConfigLog = NodeConfigLog
  { _nodeConfigLog_output :: Maybe Text
  , _nodeConfigLog_level :: Maybe Text
  , _nodeConfigLog_rules :: Maybe Text
  , _nodeConfigLog_template :: Maybe Text
  }

data NodeConfigShell = NodeConfigShell
  { _nodeConfigShell_peerValidator :: Maybe NodeConfigShellPeerValidator
  , _nodeConfigShell_blockValidator :: Maybe NodeConfigShellBlockValidator
  , _nodeConfigShell_prevalidator :: Maybe NodeConfigShellPrevalidator
  , _nodeConfigShell_chainValidator :: Maybe NodeConfigShellChainValidator
  }

data NodeConfigShellPeerValidator = NodeConfigShellPeerValidator
  { _nodeConfigShellPeerValidator_blockHeaderRequestTimeout :: Maybe Double
  , _nodeConfigShellPeerValidator_blockOperationsRequestTimeout :: Maybe Double
  , _nodeConfigShellPeerValidator_protocolRequestTimeout :: Maybe Double
  , _nodeConfigShellPeerValidator_newHeadRequestTimeout :: Maybe Double
  , _nodeConfigShellPeerValidator_workerBacklogSize :: Maybe Word16
  , _nodeConfigShellPeerValidator_workerBacklogLevel :: Maybe Text
  , _nodeConfigShellPeerValidator_workerZombieLifetime :: Maybe Double
  , _nodeConfigShellPeerValidator_workerZombieMemory :: Maybe Double
  }

data NodeConfigShellBlockValidator = NodeConfigShellBlockValidator
  { _nodeConfigShellBlockValidator_protocolRequestTimeout :: Maybe Double
  , _nodeConfigShellBlockValidator_workerBacklogSize :: Maybe Word16
  , _nodeConfigShellBlockValidator_workerBacklogLevel :: Maybe Text
  , _nodeConfigShellBlockValidator_workerZombieLifetime :: Maybe Double
  , _nodeConfigShellBlockValidator_workerZombieMemory :: Maybe Double
  }
data NodeConfigShellPrevalidator = NodeConfigShellPrevalidator
  { _nodeConfigShellPrevalidator_operationsRequestTimeout :: Maybe Double
  , _nodeConfigShellPrevalidator_maxRefusedOperations :: Maybe Word16
  , _nodeConfigShellPrevalidator_workerBacklogSize :: Maybe Word16
  , _nodeConfigShellPrevalidator_workerBacklogLevel :: Maybe Text
  , _nodeConfigShellPrevalidator_workerZombieLifetime :: Maybe Double
  , _nodeConfigShellPrevalidator_workerZombieMemory :: Maybe Double
  }
data NodeConfigShellChainValidator = NodeConfigShellChainValidator
  { _nodeConfigShellChainValidator_bootstrapThreshold :: Maybe Word8
  , _nodeConfigShellChainValidator_workerBacklogSize :: Maybe Word16
  , _nodeConfigShellChainValidator_workerBacklogLevel :: Maybe Text
  , _nodeConfigShellChainValidator_workerZombieLifetime :: Maybe Double
  , _nodeConfigShellChainValidator_workerZombieMemory :: Maybe Double
  }

data NodeConfigFile' = NodeConfigFile'
  { _nodeConfigFile_p2p :: NodeConfigP2P
  , _nodeConfigFile_dataDir :: Maybe FilePath
  , _nodeConfigFile_rpc :: Maybe NodeConfigRPC
  , _nodeConfigFile_log :: Maybe NodeConfigLog
  , _nodeConfigFile_shell :: Maybe NodeConfigShell
  , _nodeConfigFile_network :: Maybe Text
  }

data BinaryPaths = BinaryPaths
  { _binaryPaths_nodePath :: FilePath
  , _binaryPaths_clientPath :: FilePath
  , _binaryPaths_bakerEndorserPaths :: NonEmpty (ProtocolHash, FilePath, FilePath)
  }
  deriving (Show)

concat <$> traverse (Aeson.deriveJSON tezosJsonOptions
  { Aeson.fieldLabelModifier
    = map (\case {'_' -> '-'; x -> x})
    . Aeson.fieldLabelModifier tezosJsonOptions
  , Aeson.omitNothingFields = True
    })
  [ ''NodeConfigFile'
  , ''NodeConfigLog
  , ''NodeConfigP2P
  , ''NodeConfigRPC
  , ''NodeConfigShell
  , ''NodeConfigShellBlockValidator
  , ''NodeConfigShellChainValidator
  , ''NodeConfigShellPeerValidator
  , ''NodeConfigShellPrevalidator
  , ''BinaryPaths
  ]
