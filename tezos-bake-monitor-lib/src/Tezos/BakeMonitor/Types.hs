{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Tezos.BakeMonitor.Types where

import Control.Lens.TH
import Data.Aeson (Value, ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Time.Clock
import Data.Typeable
import GHC.Generics

data Report = Report
  { _report_counts :: Count
  , _report_last_baked :: [Baked]
  , _report_errors :: [Error]
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Report
instance ToJSON Report

data Count = Count
  { _count_selected :: !Integer
  , _count_injected :: !Integer
  , _count_errors :: !Integer
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Count
instance ToJSON Count

data Baked = Baked
  { _baked_seq :: !Integer
  , _baked_hash :: Text
  , _baked_time :: UTCTime
  , _baked_block :: Maybe Value
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Baked
instance ToJSON Baked

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Error
instance ToJSON Error

makeLenses 'Report
makeLenses 'Count
makeLenses 'Baked
makeLenses 'Error
