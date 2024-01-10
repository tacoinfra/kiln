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

import qualified Tezos.Oxford.Account as Oxford
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountOxford Oxford.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountOxford a -> a ^. Oxford.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountOxford <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoOxford Oxford.DelegateInfo

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoOxford <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_fullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_frozenDeposits

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_stakingBalance

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_delegatedBalance

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_deactivated

delegateInfoCrossCompat_activeConsensusKey :: Getter DelegateInfoCrossCompat PublicKeyHash
delegateInfoCrossCompat_activeConsensusKey = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_activeConsensusKey

delegateInfoCrossCompat_pendingConsensusKeys :: Getter DelegateInfoCrossCompat [Oxford.PendingConsensusKey]
delegateInfoCrossCompat_pendingConsensusKeys = to $ \case
  DelegateInfoOxford di -> di ^. Oxford.delegateInfo_pendingConsensusKeys

data BakingRightsCrossCompat
  = BakingRightsOxford Oxford.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsOxford <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsOxford e -> e ^. Oxford.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsOxford e -> e ^. Oxford.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat Oxford.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsOxford e -> e ^. Oxford.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsOxford e -> e ^. Oxford.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsOxford Oxford.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsOxford <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsOxford er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsOxford e -> e ^. Oxford.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsOxford e -> view Oxford.endorsingRightsDelegateInfo_delegate <$> e ^. Oxford.endorsingRights_delegates
