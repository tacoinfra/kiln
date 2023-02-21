{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Tezos.Mumbai.Operation where

import Control.Lens (traversed, (^.), (^..))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson
import Data.Int (Int32)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Debug.Trace (trace)
import GHC.Word

import Tezos.Common.Accusation
import Tezos.Common.BalanceUpdate
import Tezos.Common.Base58Check
import Tezos.Common.Json
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash (PublicKeyHash)
import Tezos.Common.BlockHeader

-- | "operation": {
data Operation = Operation
  { _operation_hash :: OperationHash             --  "hash": { "$ref": "#/definitions/Operation_hash" },
  , _operation_contents :: Seq OperationContents -- "contents": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.operation_contents_and_result" } },
  }
  deriving (Eq, Ord, Show, Typeable)

--
-- | "operation.alpha.operation_contents_and_result": {
data OperationContents
  = OperationContents_Endorsement                  OperationContentsEndorsement
  | OperationContents_DoubleEndorsementEvidence    OperationContentsDoubleEndorsementEvidence
  | OperationContents_DoubleBakingEvidence         OperationContentsDoubleBakingEvidence
  | OperationContents_DoublePreendorsementEvidence OperationContentsDoublePreendorsementEvidence
  | OperationContents_ActivateAccount
  | OperationContents_Proposals
  | OperationContents_Ballot
  | OperationContents_Reveal
  | OperationContents_Transaction
  | OperationContents_Origination
  | OperationContents_Delegation
  | OperationContents_FailingNoop
  | OperationContents_RegisterGlobalConstant
  | OperationContents_SetDepositsLimit
  | OperationContents_Preendorsement
  | OperationContents_SeedNonceRevelation
  | OperationContents_Unknown
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "endorsement_with_slot" ] },
data OperationContentsEndorsement = OperationContentsEndorsement
  { _operationContentsEndorsement_slot :: Word16 -- "slot": { "type": integer ∈ [0, 2^16-1] }
  , _operationContentsEndorsement_level :: RawLevel
  , _operationContentsEndorsement_round :: RawLevel
  , _operationContentsEndorsement_blockPayloadHash :: PayloadHash
  , _operationContentsEndorsement_metadata :: EndorsementMetadata
  } deriving (Eq, Ord, Show, Typeable)

data EndorsementMetadata = EndorsementMetadata
  { _endorsementMetadata_delegate :: PublicKeyHash--  "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _endorsementMetadata_endorsementPower :: Int32 --  "endorsement_power": integer ∈ [-2^30, 2^30]
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_baking_evidence" ] },
data OperationContentsDoubleBakingEvidence = OperationContentsDoubleBakingEvidence
  { _operationContentsDoubleBakingEvidence_bh1 :: BlockHeaderFull --  "bh1": { "$ref": "#/definitions/block_header.alpha.full_header" },
  , _operationContentsDoubleBakingEvidence_bh2 :: BlockHeaderFull --  "bh2": { "$ref": "#/definitions/block_header.alpha.full_header" },
  , _operationContentsDoubleBakingEvidence_metadata :: DoubleBakingEvidenceMetadata
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleBakingEvidenceMetadata = DoubleBakingEvidenceMetadata
  { _doubleBakingEvidenceMetadata_balanceUpdates :: Seq BalanceUpdate --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | Contents that is similar for endorsement and preendorsement operations, see
-- '$012-Psithaca.inlined.endorsement_mempool.contents' and '$012-Psithaca.inlined.preendorsement.contents'.
data EndorsementLikeContents = EndorsementLikeContents
  { _endorsementLikeContents_slot :: Word16
  , _endorsementLikeContents_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } },
  , _endorsementLikeContents_round :: RawLevel
  , _endorsementLikeContents_blockPayloadHash :: PayloadHash
  }
  deriving (Eq, Ord, Show, Typeable)

-- | Contents that is similar for inlined endorsements and preendorsements, see
-- '$012-Psithaca.inlined.endorsement' and '$012-Psithaca.inlined.preendorsement'.
data InlinedEndorsementLike = InlinedEndorsementLike
  { _inlinedEndorsementLike_branch :: BlockHash--  "branch": { "$ref": "#/definitions/block_hash" },
  , _inlinedEndorsementLike_operations :: EndorsementLikeContents --  "operations": { "$ref": "#/definitions/inlined.endorsement.contents" },
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_endorsement_evidence" ] },
data OperationContentsDoubleEndorsementEvidence = OperationContentsDoubleEndorsementEvidence
  { _operationContentsDoubleEndorsementEvidence_op1 :: InlinedEndorsementLike --  "op1": { "$ref": "#/definitions/inlined.endorsement" },
  , _operationContentsDoubleEndorsementEvidence_op2 :: InlinedEndorsementLike --  "op2": { "$ref": "#/definitions/inlined.endorsement" },
  , _operationContentsDoubleEndorsementEvidence_metadata :: DoubleEndorsementEvidenceMetadata
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleEndorsementEvidenceMetadata = DoubleEndorsementEvidenceMetadata
  { _doubleEndorsementEvidenceMetadata_balanceUpdates :: Seq BalanceUpdate --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_preendorsement_evidence" ] },
data OperationContentsDoublePreendorsementEvidence = OperationContentsDoublePreendorsementEvidence
  { _operationContentsDoublePreendorsementEvidence_op1 :: InlinedEndorsementLike --  "op1": { "$ref": "#/definitions/inlined.preendorsement" },
  , _operationContentsDoublePreendorsementEvidence_op2 :: InlinedEndorsementLike --  "op2": { "$ref": "#/definitions/inlined.preendorsement" },
  , _operationContentsDoublePreendorsementEvidence_metadata :: DoublePreendorsementEvidenceMetadata
  }
  deriving (Eq, Ord, Show, Typeable)

data DoublePreendorsementEvidenceMetadata = DoublePreendorsementEvidenceMetadata
  { _doublePreendorsementEvidenceMetadata_balanceUpdates :: Seq BalanceUpdate --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON OperationContents where
  parseJSON = withObject "Operation" $ \v -> do
    kind :: Text <- v .: "kind"
    case kind of
      "endorsement"                    -> OperationContents_Endorsement                  <$> parseJSON (Object v)
      "double_endorsement_evidence"    -> OperationContents_DoubleEndorsementEvidence    <$> parseJSON (Object v)
      "double_baking_evidence"         -> OperationContents_DoubleBakingEvidence         <$> parseJSON (Object v)
      "double_preendorsement_evidence" -> OperationContents_DoublePreendorsementEvidence <$> parseJSON (Object v)
      "preendorsement"                 -> pure OperationContents_Preendorsement
      "seed_nonce_revelation"          -> pure OperationContents_SeedNonceRevelation
      "activate_account"               -> pure OperationContents_ActivateAccount
      "proposals"                      -> pure OperationContents_Proposals
      "ballot"                         -> pure OperationContents_Ballot
      "reveal"                         -> pure OperationContents_Reveal
      "transaction"                    -> pure OperationContents_Transaction
      "origination"                    -> pure OperationContents_Origination
      "delegation"                     -> pure OperationContents_Delegation
      "failing_noop"                   -> pure OperationContents_FailingNoop
      "register_global_constant"       -> pure OperationContents_RegisterGlobalConstant
      "set_deposits_limit"             -> pure OperationContents_SetDepositsLimit

      "sc_rollup_add_messages"         -> pure OperationContents_Unknown
      "sc_rollup_cement"               -> pure OperationContents_Unknown
      "sc_rollup_publish"              -> pure OperationContents_Unknown
      "sc_rollup_originate"            -> pure OperationContents_Unknown
      "tx_rollup_commit"               -> pure OperationContents_Unknown
      "tx_rollup_dispatch_tickets"     -> pure OperationContents_Unknown
      "tx_rollup_finalize_commitment"  -> pure OperationContents_Unknown
      "tx_rollup_origination"          -> pure OperationContents_Unknown
      "tx_rollup_rejection"            -> pure OperationContents_Unknown
      "tx_rollup_remove_commitment"    -> pure OperationContents_Unknown
      "tx_rollup_return_bond"          -> pure OperationContents_Unknown
      "tx_rollup_submit_batch"         -> pure OperationContents_Unknown
      "transfer_ticket"                -> pure OperationContents_Unknown
      "increase_paid_storage"          -> pure OperationContents_Unknown
      "vdf_revelation"                 -> pure OperationContents_Unknown
      "event"                          -> pure OperationContents_Unknown
      "update_consensus_key"           -> pure OperationContents_Unknown
      "drain_delegate"                 -> pure OperationContents_Unknown
      unknown -> trace ("Warning: Unknown operation kind " <> T.unpack unknown) $ pure OperationContents_Unknown

concat <$> traverse deriveTezosFromJson
  [ ''Operation
  , ''OperationContentsDoubleBakingEvidence
  , ''OperationContentsDoubleEndorsementEvidence
  , ''OperationContentsDoublePreendorsementEvidence
  , ''OperationContentsEndorsement
  , ''InlinedEndorsementLike
  , ''EndorsementMetadata
  , ''EndorsementLikeContents
  , ''DoubleBakingEvidenceMetadata
  , ''DoubleEndorsementEvidenceMetadata
  , ''DoublePreendorsementEvidenceMetadata
  ]
concat <$> traverse makeLenses
  [ 'Operation
  , 'OperationContentsDoubleBakingEvidence
  , 'OperationContentsDoubleEndorsementEvidence
  , 'OperationContentsDoublePreendorsementEvidence
  , 'OperationContentsEndorsement
  , 'InlinedEndorsementLike
  , 'EndorsementMetadata
  , 'EndorsementLikeContents
  , 'DoubleBakingEvidenceMetadata
  , 'DoubleEndorsementEvidenceMetadata
  , 'DoublePreendorsementEvidenceMetadata
  ]
makePrisms ''OperationContents

instance MayHaveAccusations Operation where
  getAccusations op =
    let
      opHash = _operation_hash op
    in flip mapMaybe (op ^.. operation_contents . traversed) $ \case
      OperationContents_DoubleBakingEvidence ev ->
        let
          accusedLevel = ev
            ^. operationContentsDoubleBakingEvidence_bh1
            . blockHeaderFull_level
          balanceUpdates = ev
            ^.. operationContentsDoubleBakingEvidence_metadata
            . doubleBakingEvidenceMetadata_balanceUpdates
            . traversed
        in Just $ AccusationInfo AccusationType_DoubleBake accusedLevel opHash balanceUpdates
      OperationContents_DoubleEndorsementEvidence ev ->
        let
          accusedLevel = ev
            ^. operationContentsDoubleEndorsementEvidence_op1
            . inlinedEndorsementLike_operations
            . endorsementLikeContents_level
          balanceUpdates = ev
            ^.. operationContentsDoubleEndorsementEvidence_metadata
            . doubleEndorsementEvidenceMetadata_balanceUpdates
            . traversed
        in Just $ AccusationInfo AccusationType_DoubleEndorsement accusedLevel opHash balanceUpdates
      OperationContents_DoublePreendorsementEvidence ev ->
        let
          accusedLevel = ev
            ^. operationContentsDoublePreendorsementEvidence_op1
            . inlinedEndorsementLike_operations
            . endorsementLikeContents_level
          balanceUpdates = ev
            ^.. operationContentsDoublePreendorsementEvidence_metadata
            . doublePreendorsementEvidenceMetadata_balanceUpdates
            . traversed
        in Just $ AccusationInfo AccusationType_DoublePreendorsement accusedLevel opHash balanceUpdates
      _ -> Nothing
