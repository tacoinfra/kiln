{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.V004.Lenses where

import Control.Lens.TH (makeLenses)

import Tezos.V004.Operation
import Tezos.V004.Types

concat <$> traverse makeLenses
 [ 'ActivateMetadata
 , 'ContractScript
 , 'ContractUpdate
 , 'FreezerUpdate
 , 'Level
 ]

