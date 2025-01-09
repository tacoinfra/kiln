{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains data types similat to the ones from 'Tezos.*.Account'
-- but represented as unions to provide cross compatibility between protocols
-- in case of RPC schema changes.
module Tezos.CrossCompat.Account where

import Control.Applicative ((<|>))
import Control.Lens (Getter, to, view, (^.))
import Data.Aeson
import qualified Data.Sequence as Seq
import Data.Time

import qualified Tezos.Base.Account as Base
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data AccountCrossCompat
  = AccountBase Base.Account

accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountBase a -> a ^. Base.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv =
    AccountBase <$> parseJSON jv

data DelegateInfoCrossCompat
  = DelegateInfoParis Base.DelegateInfoParis
  | DelegateInfoQuebec Base.DelegateInfoQuebec

instance FromJSON DelegateInfoCrossCompat where
  parseJSON jv =
    DelegateInfoParis <$> parseJSON jv <|> DelegateInfoQuebec <$> parseJSON jv

delegateInfoCrossCompat_balance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_balance = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_fullBalance
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_ownFullBalance

delegateInfoCrossCompat_frozenBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_frozenBalance = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_frozenDeposits
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_totalStaked

delegateInfoCrossCompat_stakingBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_stakingBalance = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_stakingBalance
  DelegateInfoQuebec di ->
    let totalStaked = di ^. Base.delegateInfoQuebec_totalStaked
        totalDelegated = di ^. Base.delegateInfoQuebec_totalDelegated
    in totalStaked + totalDelegated

delegateInfoCrossCompat_delegatedBalance :: Getter DelegateInfoCrossCompat Tez
delegateInfoCrossCompat_delegatedBalance = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_delegatedBalance
  DelegateInfoQuebec di ->
    let externalStaked = di ^. Base.delegateInfoQuebec_externalStaked
        externalDelegated = di ^. Base.delegateInfoQuebec_externalDelegated
    in externalStaked + externalDelegated

delegateInfoCrossCompat_gracePeriod :: Getter DelegateInfoCrossCompat Cycle
delegateInfoCrossCompat_gracePeriod = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_gracePeriod
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_gracePeriod

delegateInfoCrossCompat_deactivated :: Getter DelegateInfoCrossCompat Bool
delegateInfoCrossCompat_deactivated = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_deactivated
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_deactivated

delegateInfoCrossCompat_activeConsensusKey :: Getter DelegateInfoCrossCompat PublicKeyHash
delegateInfoCrossCompat_activeConsensusKey = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_activeConsensusKey
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_activeConsensusKey

delegateInfoCrossCompat_pendingConsensusKeys :: Getter DelegateInfoCrossCompat [Base.PendingConsensusKey]
delegateInfoCrossCompat_pendingConsensusKeys = to $ \case
  DelegateInfoParis di -> di ^. Base.delegateInfoParis_pendingConsensusKeys
  DelegateInfoQuebec di -> di ^. Base.delegateInfoQuebec_pendingConsensusKeys

data BakingRightsCrossCompat
  = BakingRightsBase Base.BakingRights

instance FromJSON BakingRightsCrossCompat where
  parseJSON jv =
    BakingRightsBase <$> parseJSON jv

bakingRightsCrossCompat_level :: Getter BakingRightsCrossCompat RawLevel
bakingRightsCrossCompat_level = to $ \case
  BakingRightsBase e -> e ^. Base.bakingRights_level

bakingRightsCrossCompat_delegate :: Getter BakingRightsCrossCompat PublicKeyHash
bakingRightsCrossCompat_delegate = to $ \case
  BakingRightsBase e -> e ^. Base.bakingRights_delegate

bakingRightsCrossCompat_round :: Getter BakingRightsCrossCompat Base.Round
bakingRightsCrossCompat_round = to $ \case
  BakingRightsBase e -> e ^. Base.bakingRights_round

bakingRightsCrossCompat_estimatedTime :: Getter BakingRightsCrossCompat (Maybe UTCTime)
bakingRightsCrossCompat_estimatedTime = to $ \case
  BakingRightsBase e -> e ^. Base.bakingRights_estimatedTime

data EndorsingRightsCrossCompat
  = EndorsingRightsBase Base.EndorsingRights

instance FromJSON EndorsingRightsCrossCompat where
  parseJSON jv =
    EndorsingRightsBase <$> parseJSON jv

instance ToJSON EndorsingRightsCrossCompat where
  toJSON = \case
    EndorsingRightsBase er -> toJSON er

endorsingRightsCrossCompat_level :: Getter EndorsingRightsCrossCompat RawLevel
endorsingRightsCrossCompat_level = to $ \case
  EndorsingRightsBase e -> e ^. Base.endorsingRights_level

endorsingRightsCrossCompat_delegates :: Getter EndorsingRightsCrossCompat (Seq.Seq PublicKeyHash)
endorsingRightsCrossCompat_delegates = to $ \case
  EndorsingRightsBase e -> view Base.endorsingRightsDelegateInfo_delegate <$> e ^. Base.endorsingRights_delegates

data DelegateParametersCrossCompat
  = DelegateParametersBase Base.DelegateParameters

instance FromJSON DelegateParametersCrossCompat where
  parseJSON jv = DelegateParametersBase <$> parseJSON jv

instance ToJSON DelegateParametersCrossCompat where
  toJSON = \case
    DelegateParametersBase dp -> toJSON dp

delegateParametersCrossCompat_limit :: Getter DelegateParametersCrossCompat Int
delegateParametersCrossCompat_limit = to $ \case
  DelegateParametersBase dp -> dp ^. Base.delegateParameters_limitOfStakingOverBakingMillionth

delegateParametersCrossCompat_edge :: Getter DelegateParametersCrossCompat Int
delegateParametersCrossCompat_edge = to $ \case
  DelegateParametersBase dp -> dp ^. Base.delegateParameters_edgeOfBakingOverStakingBillionth
