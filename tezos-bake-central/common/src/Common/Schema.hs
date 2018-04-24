{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Data.Typeable
import Focus.Schema
import Data.Time

data Client = Client
  { _client_address :: Text, _client_updated :: Maybe UTCTime }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Client
instance FromJSON Client
instance ToJSON Client

data ClientInfo = ClientInfo
  { _clientInfo_client :: Id Client, _clientInfo_report :: Text }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId ClientInfo
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Baked = Baked
  { _baked_seq :: !Integer
  , _baked_hash :: Text
  , _baked_time :: UTCTime
  , _baked_block :: Maybe (Maybe Value) -- would like to use json value but 'instances...'
  }
  deriving (Eq, Show, Generic)

instance FromJSON Baked
instance ToJSON Baked


data Top = Top
  { _top_counts :: Count
  , _top_last_baked :: [Baked]
  , _top_errors :: [Error]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Top
instance ToJSON Top


data Count = Count
  { _count_selected :: !Integer
  , _count_injected :: !Integer
  , _count_errors :: !Integer
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Count
instance ToJSON Count

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Error
instance ToJSON Error
