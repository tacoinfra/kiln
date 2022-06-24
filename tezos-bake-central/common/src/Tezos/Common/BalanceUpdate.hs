{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Common.BalanceUpdate where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import GHC.Generics (Generic)

import Tezos.Common.Json (deriveTezosToJson)
import Tezos.Common.PublicKeyHash (PublicKeyHash)
import Tezos.Common.Tez (TezDelta)

data BalanceUpdateCategory
  = BalanceUpdateCategory_Deposits
  | BalanceUpdateCategory_Other
  deriving (Eq, Ord, Show, Typeable, Generic)

instance FromJSON BalanceUpdateCategory where
  parseJSON (Aeson.String "deposits") = pure BalanceUpdateCategory_Deposits
  parseJSON (Aeson.String category) | category `HashSet.member` balanceUpdateCategories
    = pure BalanceUpdateCategory_Other
  parseJSON (Aeson.String category) = trace ("Warning: Unknown balance update category " <> T.unpack category)
    $ pure BalanceUpdateCategory_Other
  parseJSON _ = fail "Unable to parse balance update category: String expected."

data FreezerUpdate = FreezerUpdate
  { _freezerUpdate_delegate :: PublicKeyHash
  , _freezerUpdate_change :: TezDelta
  , _freezerUpdate_category :: BalanceUpdateCategory
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data BalanceUpdate
  = BalanceUpdate_Freezer FreezerUpdate
  | BalanceUpdate_Other
  deriving (Eq, Ord, Show, Typeable, Generic)

instance FromJSON FreezerUpdate where
  parseJSON = withObject "FreezerUpdate" $ \o ->
    FreezerUpdate
      <$> o .: "delegate"
      <*> o .: "change"
      <*> o .: "category"

instance FromJSON BalanceUpdate where
  parseJSON = withObject "BalanceUpdate" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "freezer" -> BalanceUpdate_Freezer <$> parseJSON (Object o)
      k | k `HashSet.member` balanceUpdateKinds -> pure BalanceUpdate_Other
      k -> trace ("Warning: Unknown balance update kind " <> T.unpack k) $ pure BalanceUpdate_Other

balanceUpdateCategories :: HashSet Text
balanceUpdateCategories = HashSet.fromList
  [ -- list of balance update categories from
    -- "#/components/schemas/012-Psithaca.operation_metadata.alpha.balance_updates"
    "legacy_rewards"
  , "block fees"
  , "legacy_deposits"
  , "nonce revelation rewards"
  , "double signing evidence rewards"
  , "endorsing rewards"
  , "baking rewards"
  , "baking bonuses"
  , "legacy_fees"
  , "storage fees"
  , "punishments"
  , "lost endorsing rewards"
  , "subsidy"
  , "burned"
  , "commitment"
  , "bootstrap"
  , "invoice"
  , "minted"

  -- list of balance update categories from
  -- "#/components/schemas/011-PtHangz2.operation_metadata.alpha.balance_updates"
  , "deposits"
  , "fees"
  , "rewards"
  ]

balanceUpdateKinds :: HashSet Text
balanceUpdateKinds = HashSet.fromList
  [ -- list of balance update kinds from
    -- "#/components/schemas/012-Psithaca.operation_metadata.alpha.balance_updates"
    "accumulator"
  , "minted"
  , "burned"
  , "commitment"

  -- list of balance update kinds from
  -- "#/components/schemas/011-PtHangz2.operation_metadata.alpha.balance_updates"
  , "contract"
  , "freezer"
  ]

concat <$> traverse deriveTezosToJson
  [ ''BalanceUpdate
  , ''BalanceUpdateCategory
  , ''FreezerUpdate
  ]
