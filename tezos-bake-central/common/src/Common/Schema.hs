{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Schema where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics
import Data.Typeable
import Focus.Schema
import Data.Time
import Focus.Schema (Json)
import Tezos.BakeMonitor.Types

type ClientAddress = Text

data Client = Client
  { _client_address :: ClientAddress, _client_updated :: Maybe UTCTime }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Client
instance FromJSON Client
instance ToJSON Client

data ClientInfo = ClientInfo
  { _clientInfo_client :: Id Client, _clientInfo_report :: Json Report }
  deriving (Eq, Show, Generic, Typeable)

instance HasId ClientInfo
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Node = Node
  { _node_address :: ClientAddress
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Node
instance FromJSON Node
instance ToJSON Node

