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

import qualified Tezos.Nairobi.Account as Nairobi
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountNairobi Nairobi.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountNairobi a -> a ^. Nairobi.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountNairobi <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoNairobi Nairobi.DelegateInfo

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoNairobi <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_fullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_frozenDeposits

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_stakingBalance

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_delegatedBalance

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_deactivated

delegateInfoCrossCompat_activeConsensusKey :: Getter DelegateInfoCrossCompat PublicKeyHash
delegateInfoCrossCompat_activeConsensusKey = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_activeConsensusKey

delegateInfoCrossCompat_pendingConsensusKeys :: Getter DelegateInfoCrossCompat [Nairobi.PendingConsensusKey]
delegateInfoCrossCompat_pendingConsensusKeys = to $ \case
  DelegateInfoNairobi di -> di ^. Nairobi.delegateInfo_pendingConsensusKeys

data BakingRightsCrossCompat
  = BakingRightsNairobi Nairobi.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsNairobi <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsNairobi e -> e ^. Nairobi.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsNairobi e -> e ^. Nairobi.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat Nairobi.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsNairobi e -> e ^. Nairobi.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsNairobi e -> e ^. Nairobi.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsNairobi Nairobi.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsNairobi <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsNairobi er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsNairobi e -> e ^. Nairobi.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsNairobi e -> view Nairobi.endorsingRightsDelegateInfo_delegate <$> e ^. Nairobi.endorsingRights_delegates
