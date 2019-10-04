{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V004.NetworkStat where

import Control.Lens.TH (makeLenses)
import Data.Int (Int32)
#if !(MIN_VERSION_base(4,9,0))
import Data.Semigroup
#endif
import Data.Text (Text)
import Data.Typeable (Typeable)

import Tezos.Common.Json (TezosWord64, deriveTezosJson)

data NetworkStat = NetworkStat
  { _networkStat_totalSent      :: TezosWord64 -- bytes
  , _networkStat_totalRecv      :: TezosWord64 -- bytes
  , _networkStat_currentInflow  :: Int32 -- bytes/s
  , _networkStat_currentOutflow :: Int32 -- bytes/s
  } deriving (Eq, Ord, Show, Typeable)

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Typeable)

concat <$> traverse deriveTezosJson
  [ ''NetworkStat
  ]

concat <$> traverse makeLenses
 [ 'NetworkStat
 ]

