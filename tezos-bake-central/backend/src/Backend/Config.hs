{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Config where

import Control.Lens (Lens', view)
import Control.Monad.Reader (MonadReader, asks)
import Data.Either (fromRight)
import Data.Word
import Network.Mail.Mime (Address)
import System.FilePath ((</>))
import Text.URI (URI)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Common.URI (Port)
import ExtraPrelude
import Tezos.Base58Check (toBase58Text, ChainId)
import Tezos.Json

data AppConfig = AppConfig
  { _appConfig_emailFromAddress :: Address
  , _appConfig_kilnNodePort :: Port
  , _appConfig_kilnDataDir :: FilePath
  , _appConfig_kilnNodeConfig :: NodeConfigFile
  , _appConfig_chainId :: ChainId
  , _appConfig_kilnNodeCustomArgs :: Maybe Text
  }

class HasAppConfig a where
  getAppConfig :: Lens' a AppConfig

instance HasAppConfig AppConfig where
  getAppConfig = id

askAppConfig :: (HasAppConfig a, MonadReader a m) => m AppConfig
askAppConfig = asks $ view getAppConfig

kilnNodeURI :: AppConfig -> URI
kilnNodeURI appConfig = fromRight [Uri.uri|http://127.0.0.1:8732|] $
  Uri.mkURI ("http://127.0.0.1:" <> tshow (_appConfig_kilnNodePort appConfig))

nodeDataDir :: AppConfig -> FilePath
nodeDataDir appConfig = _appConfig_kilnDataDir appConfig
  </> fromMaybe (error "specify data-dir") (_nodeConfigFile_dataDir $ _appConfig_kilnNodeConfig appConfig)
  </> T.unpack (toBase58Text $ _appConfig_chainId appConfig)

tezosClientDataDir :: AppConfig -> FilePath
tezosClientDataDir appConfig = _appConfig_kilnDataDir appConfig </> "tezos-client"

defaultNodeConfigFile :: NodeConfigFile
defaultNodeConfigFile = NodeConfigFile
  { _nodeConfigFile_p2p = NodeConfigP2P
    { _nodeConfigP2P_expectedProofOfWork = Nothing
    , _nodeConfigP2P_bootstrapPeers = Nothing
    , _nodeConfigP2P_listenAddr = Nothing
    , _nodeConfigP2P_privateMode = Nothing
    , _nodeConfigP2P_disableMempool = Nothing
  }
  , _nodeConfigFile_dataDir = Just "tezos-node"
  , _nodeConfigFile_rpc = Just NodeConfigRPC
    { _nodeConfigRPC_listenAddr = Just "127.0.0.1"
    , _nodeConfigRPC_corsOrigin = Nothing
    , _nodeConfigRPC_corsHeaders = Nothing
    , _nodeConfigRPC_crt = Nothing
    , _nodeConfigRPC_key = Nothing
    }
  , _nodeConfigFile_log = Nothing
  , _nodeConfigFile_shell = Nothing
  }

data NodeConfigRPC = NodeConfigRPC
  { _nodeConfigRPC_listenAddr :: !(Maybe Text)
  , _nodeConfigRPC_corsOrigin :: !(Maybe [Text])
  , _nodeConfigRPC_corsHeaders :: !(Maybe [Text])
  , _nodeConfigRPC_crt :: !(Maybe Text)
  , _nodeConfigRPC_key :: !(Maybe Text)
  }

data NodeConfigP2PLimits = NodeConfigP2PLimits
  { _nodeConfigP2PLimits_connectionTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_authenticationTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_minConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_expectedConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_backlog :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxIncomingConnections :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxDownloadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_maxUploadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_swapLinger :: !(Maybe Double)
  , _nodeConfigP2PLimits_binaryChunksSize :: !(Maybe Word8)
  , _nodeConfigP2PLimits_readBufferSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_readQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_writeQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingAppMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_outgoingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_knownPointsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_knownPeerIdsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxKnownPoints :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_maxKnownPeerIds :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_greylistTimeout :: !(Maybe Int)
  }

data NodeConfigP2P = NodeConfigP2P
  { _nodeConfigP2P_expectedProofOfWork :: !(Maybe Double)
  , _nodeConfigP2P_bootstrapPeers :: !(Maybe [Text])
  , _nodeConfigP2P_listenAddr :: !(Maybe Text)
  , _nodeConfigP2P_privateMode :: !(Maybe Bool)
  , _nodeConfigP2P_disableMempool :: !(Maybe Bool)
  }
data NodeConfigLog = NodeConfigLog
  { _nodeConfigLog_output :: !(Maybe Text)
  , _nodeConfigLog_level :: !(Maybe Text)
  , _nodeConfigLog_rules :: !(Maybe Text)
  , _nodeConfigLog_template :: !(Maybe Text)
  }

data NodeConfigShell = NodeConfigShell
  { _nodeConfigShell_peerValidator :: !(Maybe NodeConfigShellPeerValidator)
  , _nodeConfigShell_blockValidator :: !(Maybe NodeConfigShellBlockValidator)
  , _nodeConfigShell_prevalidator :: !(Maybe NodeConfigShellPrevalidator)
  , _nodeConfigShell_chainValidator :: !(Maybe NodeConfigShellChainValidator)
  }

data NodeConfigShellPeerValidator = NodeConfigShellPeerValidator
  { _nodeConfigShellPeerValidator_blockHeaderRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_blockOperationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_newHeadRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPeerValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPeerValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigShellBlockValidator = NodeConfigShellBlockValidator
  { _nodeConfigShellBlockValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellBlockValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellBlockValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellPrevalidator = NodeConfigShellPrevalidator
  { _nodeConfigShellPrevalidator_operationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_maxRefusedOperations :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPrevalidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellChainValidator = NodeConfigShellChainValidator
  { _nodeConfigShellChainValidator_bootstrapThreshold :: !(Maybe Word8)
  , _nodeConfigShellChainValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellChainValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellChainValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellChainValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigFile = NodeConfigFile
  { _nodeConfigFile_p2p :: !NodeConfigP2P
  , _nodeConfigFile_dataDir :: !(Maybe FilePath)
  , _nodeConfigFile_rpc :: !(Maybe NodeConfigRPC)
  , _nodeConfigFile_log :: !(Maybe NodeConfigLog)
  , _nodeConfigFile_shell :: !(Maybe NodeConfigShell)
  }

concat <$> traverse (Aeson.deriveJSON tezosJsonOptions
  { Aeson.fieldLabelModifier
    = map (\case {'_' -> '-'; x -> x})
    . Aeson.fieldLabelModifier tezosJsonOptions
  , Aeson.omitNothingFields = True
    })
  [ ''NodeConfigFile
  , ''NodeConfigLog
  , ''NodeConfigP2P
  , ''NodeConfigRPC
  , ''NodeConfigShell
  , ''NodeConfigShellBlockValidator
  , ''NodeConfigShellChainValidator
  , ''NodeConfigShellPeerValidator
  , ''NodeConfigShellPrevalidator
  ]
