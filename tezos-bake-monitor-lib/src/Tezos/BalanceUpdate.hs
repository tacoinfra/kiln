{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.BalanceUpdate where

import Control.Lens
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Text (Text)
import Data.Map(Map)
import Data.Monoid(Sum(..))
import Data.Group
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

import Tezos.Contract
import Tezos.PublicKeyHash
import Tezos.Tez
import Tezos.Level
import Tezos.Json

data FreezerCategory
   = FreezerCategory_Rewards --  *category": { "type": "string", "enum": [ "rewards" ] },
   | FreezerCategory_Fees --  *category": { "type": "string", "enum": [ "fees" ] },
   | FreezerCategory_Deposits --  *category": { "type": "string", "enum": [ "deposits" ] },
  deriving (Eq, Ord, Show, Typeable)

instance FromJSONKey FreezerCategory where
instance ToJSONKey FreezerCategory where

data ContractUpdate = ContractUpdate
  { _contractUpdate_contract :: !ContractId --  *contract": { "$ref": "#/definitions/contract_id" },
  , _contractUpdate_change :: !Tez --  *change": { "$ref": "#/definitions/int64" } },
  }
  deriving (Eq, Ord, Show, Typeable)

data FreezerUpdate = FreezerUpdate
  { _freezerUpdate_category :: !FreezerCategory --  "category": { "type": "string", "enum": ... }
  , _freezerUpdate_delegate :: !PublicKeyHash --  *delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
    -- yes, cycle, in spite of the name!
  , _freezerUpdate_level :: !Cycle --  *level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _freezerUpdate_change :: !Tez --  *change": { "$ref": "#/definitions/int64" }
  }
  deriving (Eq, Ord, Show, Typeable)

data BalanceUpdate
   = BalanceUpdate_Contract ContractUpdate
   | BalanceUpdate_Freezer FreezerUpdate
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON BalanceUpdate where
  parseJSON = withObject "BalanceUpdate" $ \v -> do
    kind :: Text <- v .: "kind"
    case kind of
      "contract" -> BalanceUpdate_Contract <$> parseJSON (Object v)
      "freezer" -> BalanceUpdate_Freezer <$> parseJSON (Object v)
      bad -> fail $ "wrong kind:" <> show bad

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
  { _balance_spendable :: g
  , _balance_frozen :: Map Cycle (Map FreezerCategory g)
  }
  deriving (Eq, Ord, Show, Typeable)
type Balance = Balance' (Sum Tez)

instance Semigroup g => Semigroup (Balance' g) where
  Balance xs xf <> Balance ys yf = Balance (xs <> ys) (Map.unionWith (Map.unionWith (<>)) xf yf)
instance (Semigroup g, Monoid g) => Monoid (Balance' g) where
  mempty = Balance mempty (Map.empty)
  mappend = (<>)
instance (Semigroup g, Group g) => Group (Balance' g) where
  invert (Balance xs xf) = Balance (invert xs) (fmap invert <$> xf)

newtype Balances = Balances {unBalances :: Map ContractId Balance}
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup Balances where
  Balances x <> Balances y = Balances $ Map.unionWith (<>) x y
instance Monoid Balances where
  mempty = Balances $ Map.empty
  mappend = (<>)
instance Group Balances where
  invert = Balances . fmap invert . unBalances

getBalanceChanges :: HasBalanceUpdates a => a -> Balances
getBalanceChanges = views balanceUpdates toBalance
  where
    toBalance :: BalanceUpdate -> Balances
    toBalance (BalanceUpdate_Contract (ContractUpdate contract change)) =
      Balances (Map.singleton contract (Balance (Sum change) $ Map.empty))
    toBalance (BalanceUpdate_Freezer (FreezerUpdate category delegate lvl change)) =
      Balances (Map.singleton (Implicit delegate) (Balance mempty $ Map.singleton lvl $ Map.singleton category $ Sum change))



concat <$> traverse deriveTezosJson
  [ ''ContractUpdate
  , ''FreezerUpdate
  , ''FreezerCategory
  , ''Balance'
  ]
