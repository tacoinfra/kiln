{-# LANGUAGE TemplateHaskell #-}

-- | Response data for /config endpoint.
module Tezos.Common.NodeConfig where

import Control.Lens.TH (makeLenses)
import Data.Word (Word8)

import Tezos.Common.Json (deriveTezosFromJson)

-- Note: we may add more fields if needed.
data NodeConfig = NodeConfig
  { _nodeConfig_shell :: Maybe NodeConfigShell
  } deriving (Show)

data NodeConfigShell = NodeConfigShell
  { _shell_chainValidator :: Maybe NodeConfigShellChainValidator
  } deriving (Show)

data NodeConfigShellChainValidator = NodeConfigShellChainValidator
  { _chainValidator_synchronisationThreshold :: Maybe Word8
  } deriving (Show)

concat <$> traverse deriveTezosFromJson
  [ ''NodeConfig
  , ''NodeConfigShell
  , ''NodeConfigShellChainValidator
  ]

concat <$> traverse makeLenses
 [ 'NodeConfig
 , 'NodeConfigShell
 , 'NodeConfigShellChainValidator
 ]
