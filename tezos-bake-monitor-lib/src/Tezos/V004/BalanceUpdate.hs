{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V004.BalanceUpdate where

import Control.Applicative ((<|>))
import Control.Lens (Traversal', views)
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid (Sum(..))
import Data.Group
import Data.Typeable (Typeable)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Tezos.V004.Contract
import Tezos.Common.Json
import Tezos.V004.Level
import Tezos.V004.PublicKeyHash
import Tezos.V004.Tez

data FreezerCategory
   = FreezerCategory_Rewards --  *category": { "type": "string", "enum": [ "rewards" ] },
   | FreezerCategory_Fees --  *category": { "type": "string", "enum": [ "fees" ] },
   | FreezerCategory_Deposits --  *category": { "type": "string", "enum": [ "deposits" ] },
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSONKey FreezerCategory
instance ToJSONKey FreezerCategory

data ContractUpdate = ContractUpdate
  { _contractUpdate_contract :: !ContractId --  *contract": { "$ref": "#/definitions/contract_id" },
  , _contractUpdate_change :: !TezDelta --  *change": { "$ref": "#/definitions/int64" } },
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

data FreezerUpdate = FreezerUpdate
  { _freezerUpdate_category :: !FreezerCategory --  "category": { "type": "string", "enum": ... }
  , _freezerUpdate_delegate :: !PublicKeyHash --  *delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  -- Protocol 003: *level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  -- Protocol 004: *cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _freezerUpdate_cycle :: !Cycle
  , _freezerUpdate_change :: !TezDelta --  *change": { "$ref": "#/definitions/int64" }
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON FreezerUpdate where
  parseJSON = withObject "FreezerUpdate" $ \o ->
    FreezerUpdate
      <$> o .: "category"
      <*> o .: "delegate"
      <*> (o .: "cycle" <|> o .: "level")
      <*> o .: "change"

data BalanceUpdate
   = BalanceUpdate_Contract ContractUpdate
   | BalanceUpdate_Freezer FreezerUpdate
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BalanceUpdate where
  parseJSON = withObject "BalanceUpdate" $ \v -> do
    kind :: Text <- v .: "kind"
    case kind of
      "contract" -> BalanceUpdate_Contract <$> parseJSON (Object v)
      "freezer" -> BalanceUpdate_Freezer <$> parseJSON (Object v)
      bad -> fail $ "wrong kind: " <> show bad
instance ToJSON BalanceUpdate where
  toJSON (BalanceUpdate_Contract x) = case toJSON x of
    Object xs -> Object $ xs <> HashMap.singleton "kind" "contract"
    _ -> error "ToJSON did not return an object"
  toJSON (BalanceUpdate_Freezer x) = case toJSON x of
    Object xs -> Object $ xs <> HashMap.singleton "kind" "freezer"
    _ -> error "ToJSON did not return an object"

class HasBalanceUpdates a where
  balanceUpdates :: Traversal' a BalanceUpdate


data Balance' g = Balance
  { _balance_spendable :: !g
  , _balance_frozen :: !(Map Cycle (Map FreezerCategory g))
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
type Balance = Balance' (Sum TezDelta)

instance Semigroup g => Semigroup (Balance' g) where
  Balance xs xf <> Balance ys yf = Balance (xs <> ys) (Map.unionWith (Map.unionWith (<>)) xf yf)
instance (Semigroup g, Monoid g) => Monoid (Balance' g) where
  mempty = Balance mempty Map.empty
  mappend = (<>)
instance (Semigroup g, Group g) => Group (Balance' g) where
  invert (Balance xs xf) = Balance (invert xs) (fmap invert <$> xf)

newtype Balances = Balances {unBalances :: Map ContractId Balance}
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Semigroup Balances where
  Balances x <> Balances y = Balances $ Map.unionWith (<>) x y
instance Monoid Balances where
  mempty = Balances Map.empty
  mappend = (<>)
instance Group Balances where
  invert = Balances . fmap invert . unBalances

getBalanceChanges :: HasBalanceUpdates a => a -> Balances
getBalanceChanges = views balanceUpdates toBalance
  where
    toBalance :: BalanceUpdate -> Balances
    toBalance (BalanceUpdate_Contract (ContractUpdate contract change)) =
      Balances (Map.singleton contract (Balance (Sum change) Map.empty))
    toBalance (BalanceUpdate_Freezer (FreezerUpdate category delegate lvl change)) =
      Balances (Map.singleton (Implicit delegate) (Balance mempty $ Map.singleton lvl $ Map.singleton category $ Sum change))

concat <$> traverse deriveTezosJson
  [ ''ContractUpdate
  , ''FreezerCategory
  , ''Balance'
  ]
deriveTezosToJson ''FreezerUpdate
