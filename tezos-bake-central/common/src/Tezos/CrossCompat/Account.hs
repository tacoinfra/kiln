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

import qualified Tezos.Mumbai.Account as Mumbai
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountMumbai Mumbai.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountMumbai a -> a ^. Mumbai.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountMumbai <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoMumbai Mumbai.DelegateInfo

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoMumbai <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_fullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_frozenDeposits

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_stakingBalance

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_delegatedBalance

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_deactivated

delegateInfoCrossCompat_activeConsensusKey :: Getter DelegateInfoCrossCompat PublicKeyHash
delegateInfoCrossCompat_activeConsensusKey = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_activeConsensusKey

delegateInfoCrossCompat_pendingConsensusKeys :: Getter DelegateInfoCrossCompat [Mumbai.PendingConsensusKey]
delegateInfoCrossCompat_pendingConsensusKeys = to $ \case
  DelegateInfoMumbai di -> di ^. Mumbai.delegateInfo_pendingConsensusKeys

data BakingRightsCrossCompat
  = BakingRightsMumbai Mumbai.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsMumbai <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsMumbai e -> e ^. Mumbai.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsMumbai e -> e ^. Mumbai.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat Mumbai.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsMumbai e -> e ^. Mumbai.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsMumbai e -> e ^. Mumbai.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsMumbai Mumbai.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsMumbai <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsMumbai er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsMumbai e -> e ^. Mumbai.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsMumbai e -> view Mumbai.endorsingRightsDelegateInfo_delegate <$> e ^. Mumbai.endorsingRights_delegates
