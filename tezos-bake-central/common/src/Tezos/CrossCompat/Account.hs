{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains data types similat to the ones from 'Tezos.V*.Account'
-- but represented as unions to provide cross compatibility between protocols
-- in case of RPC schema changes.
module Tezos.CrossCompat.Account where

import Control.Lens (Getter, to, view, (^.))
import Data.Aeson
import qualified Data.Sequence as Seq
import Data.Time

import qualified Tezos.Lima.Account as Lima
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountLima Lima.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountLima a -> a ^. Lima.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountLima <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoLima Lima.DelegateInfo

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoLima <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_fullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_frozenDeposits

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_stakingBalance

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_delegatedBalance

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoLima di -> di ^. Lima.delegateInfo_deactivated

data BakingRightsCrossCompat
  = BakingRightsLima Lima.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsLima <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsLima e -> e ^. Lima.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsLima e -> e ^. Lima.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat Lima.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsLima e -> e ^. Lima.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsLima e -> e ^. Lima.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsLima Lima.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsLima <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsLima er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsLima e -> e ^. Lima.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsLima e -> view Lima.endorsingRightsDelegateInfo_delegate <$> e ^. Lima.endorsingRights_delegates
