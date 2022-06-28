{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Common.NetworkStat where

{--
This is kinda weird. This could theoretically change later and need versioning, but
from what I've been told this is not changed on chain and is just part of the node
software. So if it does change later it will need versioning, but it's not really
a Protocol level difference and needs a better home then. Lets hope that any changes
there are non-breaking and are just additive.
--}

import Control.Lens.TH (makeLenses)
import Data.Int (Int32)
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
