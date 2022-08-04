{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module contains data types similat to the ones from 'Tezos.V*.Account'
-- but represented as unions to provide cross compatibility between protocols
-- in case of RPC schema changes.
module Tezos.CrossCompat.Account where

import Control.Lens (Getter, to, view, (^.))
import Data.Aeson
import qualified Data.Sequence as Seq
import Data.Time

import qualified Tezos.V014.Account as V014
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountV014 V014.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountV014 a -> a ^. V014.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountV014 <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoV014 V014.DelegateInfo

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoV014 <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_fullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_frozenDeposits

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_stakingBalance

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_delegatedBalance

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoV014 di -> di ^. V014.delegateInfo_deactivated

data BakingRightsCrossCompat
  = BakingRightsV014 V014.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsV014 <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsV014 e -> e ^. V014.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsV014 e -> e ^. V014.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat V014.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsV014 e -> e ^. V014.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsV014 e -> e ^. V014.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsV014 V014.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsV014 <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsV014 er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsV014 e -> e ^. V014.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsV014 e -> view V014.endorsingRightsDelegateInfo_delegate <$> e ^. V014.endorsingRights_delegates
