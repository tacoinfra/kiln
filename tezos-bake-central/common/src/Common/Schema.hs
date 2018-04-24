{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Data.Typeable
import Focus.Schema

data Client = Client
  { _client_address :: Text }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Client

data ClientInfo = ClientInfo
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Client
instance FromJSON ClientInfo
instance ToJSON Client
instance ToJSON ClientInfo