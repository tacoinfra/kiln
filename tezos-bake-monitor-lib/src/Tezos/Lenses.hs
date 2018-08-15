{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Lenses where

import Control.Lens.TH (makeLenses)

import Tezos.NodeRPC
import Tezos.Operation
import Tezos.Types

concat <$> traverse makeLenses
 [ 'Account
 , 'AccountDelegate
 , 'ActivateMetadata
 , 'BakingRights
 , 'ContractScript
 , 'ContractUpdate
 , 'EndorsingRights
 , 'FreezerUpdate
 , 'Level
 , 'NetworkStat
 , 'NodeRPCContext
 , 'ProtoInfo
 ]

