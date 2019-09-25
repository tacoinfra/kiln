{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Account where

import Control.Applicative ((<|>))
import Control.Lens (Getter, (^.), to)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Data.Time
import Data.Typeable
import Data.Word
import qualified Data.Sequence as Seq

import Tezos.BlockHeader (Priority(..))
import Tezos.Contract
import Tezos.Json
import Tezos.Level
import Tezos.PublicKey
import Tezos.PublicKeyHash
import Tezos.Tez

data AccountDelegate = AccountDelegate
  { _accountDelegate_setable :: !Bool -- "setable": { "type": "boolean" },
  , _accountDelegate_value :: !(Maybe PublicKeyHash) -- "value": { "$ref": "#/definitions/Signature.Public_key_hash" }
  } deriving (Show, Eq, Ord, Typeable)

data AccountDelegateVersioned
  = AccountDelegateAthens !AccountDelegate -- { "type": "object", "properties": {
  | AccountDelegateBabylon !(Maybe PublicKeyHash) -- "value": { "$ref": "#/definitions/Signature.Public_key_hash" }
  deriving (Show, Eq, Ord, Typeable)

instance ToJSON AccountDelegateVersioned where
  toJSON v = case v of
    AccountDelegateAthens ad -> toJSON ad
    AccountDelegateBabylon pkh -> toJSON pkh

instance FromJSON AccountDelegateVersioned where
  parseJSON v = (AccountDelegateBabylon <$> parseJSON v) <|> (AccountDelegateAthens <$> parseJSON v)

data Account = Account
  { _account_delegate :: !AccountDelegateVersioned -- "delegate": (different for babylon and athens)
  , _account_balance :: !Tez -- "2052452947621" "balance": { "$ref": "#/definitions/mutez" },
  , _account_script :: !(Maybe ContractScript) -- "script": { "$ref": "#/definitions/scripted.contracts" },
  , _account_counter :: !TezosWord64 -- 1540 "counter": { "$ref": "#/definitions/positive_bignum" }
  } deriving (Show, Eq, Ord, Typeable)

data BakingRights = BakingRights
  { _bakingRights_level :: !RawLevel
  , _bakingRights_delegate :: !PublicKeyHash
  , _bakingRights_priority :: !Priority
  , _bakingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data EndorsingRights = EndorsingRights
  { _endorsingRights_level :: !RawLevel
  , _endorsingRights_delegate :: !PublicKeyHash
  , _endorsingRights_slots :: !(Seq.Seq Word8)
  , _endorsingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data FrozenBalanceByCycle = FrozenBalanceByCycle
  { _frozenBalanceByCycle_cycle :: !Cycle
  , _frozenBalanceByCycle_deposit :: !Tez
  , _frozenBalanceByCycle_fees :: !Tez
  , _frozenBalanceByCycle_rewards :: !Tez
  } deriving (Eq, Ord, Show)

data DelegateInfo = DelegateInfo
  { _delegateInfo_balance :: !Tez
  , _delegateInfo_frozenBalance :: !Tez
  , _delegateInfo_frozenBalanceByCycle :: !(Seq.Seq FrozenBalanceByCycle)
  , _delegateInfo_stakingBalance :: !Tez
  , _delegateInfo_delegatedContracts :: !(Seq.Seq ContractId)
  , _delegateInfo_delegatedBalance :: !Tez
  , _delegateInfo_deactivated :: !Bool
  , _delegateInfo_gracePeriod :: !Cycle
  }

data ManagerKey = ManagerKey
  { _managerKey_manager :: !PublicKeyHash
  , _managerKey_key :: !(Maybe PublicKey)
  } deriving (Show, Eq, Ord)

concat <$> traverse deriveTezosJson
  [ ''Account
  , ''AccountDelegate
  , ''BakingRights
  , ''DelegateInfo
  , ''EndorsingRights
  , ''FrozenBalanceByCycle
  , ''ManagerKey
  ]

concat <$> traverse makeLenses
  [ 'Account
  , 'AccountDelegate
  , 'BakingRights
  , 'DelegateInfo
  , 'EndorsingRights
  , 'FrozenBalanceByCycle
  , 'ManagerKey
  ]

concat <$> traverse makePrisms
  [ ''AccountDelegateVersioned
  ]

account_delegatePkh :: Getter Account (Maybe PublicKeyHash)
account_delegatePkh = account_delegate . to unPkh
  where
    unPkh = \case
      AccountDelegateAthens ad -> ad ^. accountDelegate_value
      AccountDelegateBabylon pkh -> pkh
