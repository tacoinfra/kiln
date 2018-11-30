{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Lenses where

import Control.Lens.TH (makeLenses)

import Tezos.Operation
import Tezos.Types

concat <$> traverse makeLenses
 [ 'ActivateMetadata
 , 'ContractScript
 , 'ContractUpdate
 , 'FreezerUpdate
 , 'Level
 , 'ProtoInfo
 ]

