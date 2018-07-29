{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Tezos.Lenses where

import Control.Lens.TH (makeLenses)

import Tezos.NodeRPC
import Tezos.Types
import Tezos.Operation

concat <$> traverse makeLenses
 [ 'Account
 , 'AccountDelegate
 , 'ActivateMetadata
 , 'BakingRights
 , 'BlockId
 , 'ContractScript
 , 'ContractUpdate
 , 'EndorsingRights
 , 'FreezerUpdate
 , 'Level
 , 'NetworkStat
 , 'NodeRPCContext
 , 'ProtoInfo
 ]

