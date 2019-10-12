{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V004.Checkpoint where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Typeable
import GHC.Generics (Generic)

import Tezos.Common.Json
import Tezos.V004.Level
import Tezos.V004.BlockHeader

data Checkpoint = Checkpoint
  { _checkpoint_savePoint :: !RawLevel
  , _checkpoint_caboose :: !RawLevel
  , _checkpoint_historyMode :: !HistoryMode
  , _checkpoint_block :: !BlockHeaderShell
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData Checkpoint

data HistoryMode
  = HistoryMode_Archive
  | HistoryMode_Full
  | HistoryMode_Rolling
  deriving (Show, Read, Eq, Ord, Generic, Typeable)
instance NFData HistoryMode

concat <$> traverse deriveTezosJson [ ''Checkpoint, ''HistoryMode ]
concat <$> traverse makeLenses [ 'Checkpoint ]
