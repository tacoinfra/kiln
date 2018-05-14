{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Schema where

import Data.Aeson
import Data.Fixed
import Data.Text (Text)
import GHC.Generics
import Data.Typeable
import Focus.Schema
import Data.Time
import Data.Word
import Focus.Schema (Json)
import Tezos.BakeMonitor.Types

type ClientAddress = Text

data Client = Client
  { _client_address :: ClientAddress
  , _client_updated :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Client
instance FromJSON Client
instance ToJSON Client

data PendingReward = PendingReward
  { _pendingReward_client :: Id Client
  , _pendingReward_hash :: Text -- needed because we need to be able to tell that we're not adding the same reward twice
  , _pendingReward_level :: Word64
  , _pendingReward_amount :: Micro
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId PendingReward
instance FromJSON PendingReward
instance ToJSON PendingReward

data ClientInfo = ClientInfo
  { _clientInfo_client :: Id Client
  , _clientInfo_report :: Json Report
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId ClientInfo
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Node = Node
  { _node_address :: ClientAddress
  , _node_headLevel :: Maybe Word64
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Node
instance FromJSON Node
instance ToJSON Node

data Parameters = Parameters
  { _parameters_node :: Id Node
  , _parameters_protoInfo :: ProtoInfo
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Parameters
instance FromJSON Parameters
instance ToJSON Parameters