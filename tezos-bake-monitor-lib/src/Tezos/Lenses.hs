{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Tezos.Lenses where

import Data.Foldable
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal

import Tezos.NodeRPC
import Tezos.Types
import Tezos.Operation

concat <$> traverse makeLenses
 [ 'Account
 , 'AccountDelegate
 , 'ActivateMetadata
 , 'BakingRights
 , 'Block
 , 'BlockHeader
 , 'BlockMetadata
 , 'BlockId
 , 'ContractScript
 , 'ContractUpdate
 , 'DoubleBakingEvidenceMetadata
 , 'DoubleEndorsementEvidenceMetadata
 , 'EndorsementMetadata
 , 'EndorsingRights
 , 'FreezerUpdate
 , 'InlinedEndorsement
 , 'InlinedEndorsementContents
 , 'Level
 , 'ManagerOperationMetadata
 , 'MaxOperationListLength --  "max_operation_list_length": {
 , 'MonitorBlock
 , 'NetworkStat
 , 'NodeRPCContext
 , 'Operation
 , 'OperationContentsActivateAccount
 , 'OperationContentsBallot
 , 'OperationContentsDelegation
 , 'OperationContentsDoubleBakingEvidence
 , 'OperationContentsDoubleEndorsementEvidence
 , 'OperationContentsEndorsement
 , 'OperationContentsOrigination
 , 'OperationContentsProposals
 , 'OperationContentsReveal
 , 'OperationContentsSeedNonceRevelation
 , 'OperationContentsTransaction
 , 'OperationResult
 , 'OperationResultDelegation
 , 'OperationResultOrigination
 , 'OperationResultReveal
 , 'OperationResultTransaction
 , 'ProtoInfo
 , 'SeedNonceRevelationMetadata
 ]

-- balanceUpdates :: Operation -> [BalanceUpdate]
balanceUpdates :: Traversal' Operation BalanceUpdate
balanceUpdates = operation_contents . traverse . go
  where
    go :: Traversal' OperationContents BalanceUpdate
    go f = \case
      OperationContents_Endorsement op -> OperationContents_Endorsement <$> (operationContentsEndorsement_metadata . endorsementMetadata_balanceUpdates . traverse $ f ) op
      OperationContents_SeedNonceRevelation op -> OperationContents_SeedNonceRevelation <$> (operationContentsSeedNonceRevelation_metadata . seedNonceRevelationMetadata_balanceUpdates . traverse $ f) op
      OperationContents_DoubleEndorsementEvidence op -> OperationContents_DoubleEndorsementEvidence <$> (operationContentsDoubleEndorsementEvidence_metadata . doubleEndorsementEvidenceMetadata_balanceUpdates . traverse $ f) op
      OperationContents_DoubleBakingEvidence op -> OperationContents_DoubleBakingEvidence <$> (operationContentsDoubleBakingEvidence_metadata . doubleBakingEvidenceMetadata_balanceUpdates . traverse$ f) op
      OperationContents_ActivateAccount op -> OperationContents_ActivateAccount <$> (operationContentsActivateAccount_metadata . activateMetadata_balanceUpdates . traverse $ f) op

      -- have no balance consequences
      OperationContents_Proposals op -> pure $ OperationContents_Proposals op
      OperationContents_Ballot op -> pure $ OperationContents_Ballot op

      -- all have the saem fields
      OperationContents_Reveal op -> OperationContents_Reveal <$> (operationContentsReveal_metadata . mgOpFees $ f) op
      OperationContents_Transaction op -> OperationContents_Transaction <$> (operationContentsTransaction_metadata . mgOpFees $ f) op
      OperationContents_Origination op -> OperationContents_Origination <$> (operationContentsOrigination_metadata . mgOpFees $ f) op
      OperationContents_Delegation op -> OperationContents_Delegation <$> (operationContentsDelegation_metadata . mgOpFees $ f) op

    mgOpFees :: forall a. Traversal' (ManagerOperationMetadata a) BalanceUpdate
    mgOpFees = managerOperationMetadata_balanceUpdates . traverse
