{-# LANGUAGE TemplateHaskell #-}

module Tezos.TestChainStatus where

import Data.Time
import Data.Typeable

import Tezos.Base58Check
import Tezos.Json

-- | "test_chain_status": {
data TestChainStatus
  = TestChainStatus_NotRunning -- "status": { "type": "string", "enum": [ "not_running" ] }
  | TestChainStatus_Forking -- "status": { "type": "string", "enum": [ "forking" ] },
    { _testChainStatusForking_protocol :: !ProtocolHash --  "protocol": { "$ref": "#/definitions/Protocol_hash" },
    , _testChainStatusForking_expiration :: !UTCTime --  "expiration": { "$ref": "#/definitions/timestamp" }
    }
  | TestChainStatus_Running --  "status": { "type": "string", "enum": [ "running" ] },
    { _testChainStatusRunning_chainId :: !ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
    , _testChainStatusRunning_genesis :: !BlockHash --  "genesis": { "$ref": "#/definitions/block_hash" },
    , _testChainStatusRunning_protocol :: !ProtocolHash --  "protocol": { "$ref": "#/definitions/Protocol_hash" },
    , _testChainStatusRunning_expiration :: UTCTime --  "expiration": { "$ref": "#/definitions/timestamp" }
    }
  deriving (Eq, Ord, Show, Typeable)

deriveTezosJsonKind "status" ''TestChainStatus
