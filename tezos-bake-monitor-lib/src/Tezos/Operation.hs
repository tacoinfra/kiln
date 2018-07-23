{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Tezos.Operation where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable
import GHC.Word
import qualified Data.Aeson.TH as Aeson
import qualified Data.HashMap.Strict as HashMap

import Tezos.BalanceUpdate
import Tezos.Base16ByteString
import Tezos.Base58Check
import Tezos.BlockHeader
import Tezos.Contract
import Tezos.Json
import Tezos.Level
import Tezos.Micheline
import Tezos.PublicKey
import Tezos.PublicKeyHash
import Tezos.Signature
import Tezos.Tez


-- | "operation": {
data Operation = Operation
  { _operation_protocol :: !ProtocolHash -- ^         "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _operation_chainId :: !ChainId -- ^         "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _operation_hash :: !OperationHash -- ^         "hash": { "$ref": "#/definitions/Operation_hash" },
  , _operation_branch :: !BlockHash -- ^         "branch": { "$ref": "#/definitions/block_hash" },
  , _operation_contents :: !(Seq OperationContents) -- ^         "contents": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.operation_contents_and_result" } },
                                                 -- ^         "contents": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.contents" } },
  , _operation_signature :: !(Maybe Signature) -- ^         "signature": { "$ref": "#/definitions/Signature" }
  }
  deriving (Eq, Ord, Show, Typeable)
--
-- | "operation.alpha.operation_contents_and_result": {
data OperationContents
  = OperationContents_Endorsement                 !OperationContentsEndorsement
  | OperationContents_SeedNonceRevelation         !OperationContentsSeedNonceRevelation
  | OperationContents_DoubleEndorsementEvidence   !OperationContentsDoubleEndorsementEvidence
  | OperationContents_DoubleBakingEvidence        !OperationContentsDoubleBakingEvidence
  | OperationContents_ActivateAccount             !OperationContentsActivateAccount
  | OperationContents_Proposals                   !OperationContentsProposals
  | OperationContents_Ballot                      !OperationContentsBallot
  | OperationContents_Reveal                      !OperationContentsReveal
  | OperationContents_Transaction                 !OperationContentsTransaction
  | OperationContents_Origination                 !OperationContentsOrigination
  | OperationContents_Delegation                  !OperationContentsDelegation
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON OperationContents where
  parseJSON = withObject "Operation" $ \v -> do
    kind :: Text <- v .: "kind"
    case kind of
      "endorsement"                 -> OperationContents_Endorsement               <$> parseJSON (Object v)
      "seed_nonce_revelation"       -> OperationContents_SeedNonceRevelation       <$> parseJSON (Object v)
      "double_endorsement_evidence" -> OperationContents_DoubleEndorsementEvidence <$> parseJSON (Object v)
      "double_baking_evidence"      -> OperationContents_DoubleBakingEvidence      <$> parseJSON (Object v)
      "activate_account"            -> OperationContents_ActivateAccount           <$> parseJSON (Object v)
      "proposals"                   -> OperationContents_Proposals                 <$> parseJSON (Object v)
      "ballot"                      -> OperationContents_Ballot                    <$> parseJSON (Object v)
      "reveal"                      -> OperationContents_Reveal                    <$> parseJSON (Object v)
      "transaction"                 -> OperationContents_Transaction               <$> parseJSON (Object v)
      "origination"                 -> OperationContents_Origination               <$> parseJSON (Object v)
      "delegation"                  -> OperationContents_Delegation                <$> parseJSON (Object v)
      bad -> fail $ "wrong kind:" <> show bad

instance ToJSON OperationContents where
  toJSON (OperationContents_Endorsement               x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "endorsement"                 ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_SeedNonceRevelation       x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "seed_nonce_revelation"       ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_DoubleEndorsementEvidence x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "double_endorsement_evidence" ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_DoubleBakingEvidence      x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "double_baking_evidence"      ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_ActivateAccount           x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "activate_account"            ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Proposals                 x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "proposals"                   ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Ballot                    x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "ballot"                      ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Reveal                    x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "reveal"                      ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Transaction               x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "transaction"                 ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Origination               x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "origination"                 ; _ -> error "toJSON did not return an object" }
  toJSON (OperationContents_Delegation                x) = case toJSON x of { Object xs -> Object $ xs <> HashMap.singleton "kind" "delegation"                  ; _ -> error "toJSON did not return an object" }


-- | "kind": { "type": "string", "enum": [ "endorsement" ] },
data OperationContentsEndorsement = OperationContentsEndorsement
  { _operationContentsEndorsement_metadata :: EndorsementMetadata
  , _operationContentsEndorsement_level :: RawLevel -- ^ "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  }
  deriving (Eq, Ord, Show, Typeable)

data EndorsementMetadata = EndorsementMetadata
  { _endorsementMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
  , _endorsementMetadata_delegate :: !PublicKeyHash-- ^ "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _endorsementMetadata_slots :: !(Seq Word8) -- ^ "slots": { "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 255 } }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "seed_nonce_revelation" ] },
data OperationContentsSeedNonceRevelation = OperationContentsSeedNonceRevelation
  { _operationContentsSeedNonceRevelation_metadata :: SeedNonceRevelationMetadata
  , _operationContentsSeedNonceRevelation_level :: RawLevel -- ^ "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsSeedNonceRevelation_nonce :: !(Base16ByteString ByteString) -- ^ "nonce": { "type": "string", "pattern": "^[a-zA-Z0-9]+$" },
  }
  deriving (Eq, Ord, Show, Typeable)

data SeedNonceRevelationMetadata = SeedNonceRevelationMetadata
  { _seedNonceRevelationMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

data InlinedEndorsement = InlinedEndorsement
  { _inlinedEndorsement_branch :: !BlockHash-- ^ "branch": { "$ref": "#/definitions/block_hash" },
  , _inlinedEndorsement_operations :: !InlinedEndorsementContents -- ^ "operations": { "$ref": "#/definitions/inlined.endorsement.contents" },
  , _inlinedEndorsement_signature :: !(Maybe Signature) -- ^ "signature": { "$ref": "#/definitions/Signature" }
  }
  deriving (Eq, Ord, Show, Typeable)

data InlinedEndorsementContents = InlinedEndorsementContents
  { _inlinedEndorsementContents_level :: RawLevel -- ^ "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } },
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_endorsement_evidence" ] },
data OperationContentsDoubleEndorsementEvidence = OperationContentsDoubleEndorsementEvidence
  { _operationContentsDoubleEndorsementEvidence_metadata :: DoubleEndorsementEvidenceMetadata
  , _operationContentsDoubleEndorsementEvidence_op1 :: InlinedEndorsement -- ^ "op1": { "$ref": "#/definitions/inlined.endorsement" },
  , _operationContentsDoubleEndorsementEvidence_op2 :: InlinedEndorsement -- ^ "op2": { "$ref": "#/definitions/inlined.endorsement" },
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleEndorsementEvidenceMetadata = DoubleEndorsementEvidenceMetadata
  { _doubleEndorsementEvidenceMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_baking_evidence" ] },
data OperationContentsDoubleBakingEvidence = OperationContentsDoubleBakingEvidence
  { _operationContentsDoubleBakingEvidence_metadata :: !DoubleBakingEvidenceMetadata
  , _operationContentsDoubleBakingEvidence_bh1 :: !BlockHeader -- ^ "bh1": { "$ref": "#/definitions/block_header.alpha.full_header" },
  , _operationContentsDoubleBakingEvidence_bh2 :: !BlockHeader -- ^ "bh2": { "$ref": "#/definitions/block_header.alpha.full_header" },
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleBakingEvidenceMetadata = DoubleBakingEvidenceMetadata
  { _doubleBakingEvidenceMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "activate_account" ] },
data OperationContentsActivateAccount = OperationContentsActivateAccount
  { _operationContentsActivateAccount_metadata :: !ActivateMetadata
  , _operationContentsActivateAccount_pkh :: !Ed25519PublicKeyHash-- ^ "pkh": { "$ref": "#/definitions/Ed25519.Public_key_hash" },
  , _operationContentsActivateAccount_secret :: !(Base16ByteString ByteString) -- ^ "secret": { "type": "string", "pattern": "^[a-zA-Z0-9]+$" },
  }
  deriving (Eq, Ord, Show, Typeable)

data ActivateMetadata = ActivateMetadata
  { _activateMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "proposals" ] },
data OperationContentsProposals = OperationContentsProposals
  { _operationContentsProposals_metadata :: !() -- ^ "metadata": { "type": "object", "properties": {}, "additionalProperties": false }
  , _operationContentsProposals_source :: !PublicKeyHash -- ^ "source": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsProposals_period :: !RawLevel -- ^ "period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsProposals_proposals :: !(Seq ProtocolHash) -- ^ "proposals": { "type": "array", "items": { "$ref": "#/definitions/Protocol_hash" } },
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "ballot": { "type": "string", "enum": [ "nay", "yay", "pass" ] },
data Ballot
   = Ballot_Nay
   | Ballot_Yay
   | Ballot_Pass
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "ballot" ] },
data OperationContentsBallot = OperationContentsBallot
  { _operationContentsBallot_metadata :: !() -- ^ "metadata": { "type": "object", "properties": {}, "additionalProperties": false }
  , _operationContentsBallot_source :: !PublicKeyHash -- ^ "source": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsBallot_period :: !RawLevel -- ^ "period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsBallot_proposal :: !ProtocolHash -- ^ "proposal": { "$ref": "#/definitions/Protocol_hash" },
  , _operationContentsBallot_ballot :: !Ballot
  }
  deriving (Eq, Ord, Show, Typeable)

data ManagerOperationMetadata a = ManagerOperationMetadata
  { _managerOperationMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  , _managerOperationMetadata_operationResult :: !(OperationResult a) -- ^ "operation_result": { "$ref": "#/definitions/operation.alpha.operation_result.reveal" },
  -- I don't see these in the output from the nodes, seems redundant,  i'll skip them for now.
  -- , _managerOperationMetadata_internalOperationResults :: !(Seq InternalOperationResult) -- ^ "internal_operation_results": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.internal_operation_result" } }
  }
  deriving (Eq, Ord, Show, Typeable)

data OperationResultStatus
  =  OperationResultStatus_Applied     -- ^ no errors, have result
  |  OperationResultStatus_Failed      -- ^ have errors no result
  |  OperationResultStatus_Skipped     -- ^ no errors, no result
  |  OperationResultStatus_Backtracked -- ^ errors and result
  deriving (Eq, Ord, Show, Typeable)

-- | only certain combinations of status/errors/content are valid, but ignore that for now
data OperationResult a = OperationResult
  { _operationResult_status :: !OperationResultStatus
  , _operationResult_errors :: !(Maybe (Seq JsonRpcError))
  , _operationResult_content :: !(Maybe a)
  }
  deriving (Eq, Ord, Show, Typeable)

instance (Typeable a, FromJSON a) => FromJSON (OperationResult a) where
  -- parseJSON :: forall a. (Typeable a, FromJSON a)  => Value -> Aeson.Parser (OperationResult a)
  parseJSON = withObject (show $ typeRep (Proxy :: Proxy a)) $ \v -> do
        status <- v .: "status"
        let x = OperationResult status <$> case status of
              OperationResultStatus_Applied -> pure Nothing
              OperationResultStatus_Failed -> v .:? "errors"
              OperationResultStatus_Skipped -> pure Nothing
              OperationResultStatus_Backtracked -> v .:? "errors"
        x <*> case status of
          OperationResultStatus_Applied -> Just <$> parseJSON (Object v)
          OperationResultStatus_Failed -> pure Nothing
          OperationResultStatus_Skipped -> pure Nothing
          OperationResultStatus_Backtracked -> parseJSON (Object v)

instance (Typeable a, ToJSON a) => ToJSON (OperationResult a) where
  -- toJSON :: forall a. (ToJSON a, Typeable a) => OperationResult a -> Value
  toJSON x = Object (status <> errors <> content)
    where
      status = HashMap.singleton "status" (toJSON $ _operationResult_status x)
      errors = foldMap (HashMap.singleton "errors" . toJSON) $ _operationResult_errors x
      content = case toJSON (_operationResult_content x) of
          Object x' -> x'
          _ -> error ("ToJSON did not produce an object for:" <> (show $ typeRep $ (Proxy :: Proxy a)))

  -- toEncoding :: forall a. (ToJSON a, Typeable a) => OperationResult a -> Value
  -- toEncoding x = Object (status <> errors <> content)
  --   where
  --     status = HashMap.singleton "status" (toEncoding $ _operationResult_status x)
  --     errors = foldMap (HashMap.singleton "errors" . toEncoding) $ _operationResult_errors x
  --     content = case toEncoding (_operationResult_content x) of
  --         Object x' -> x'
  --         _ -> error ("ToEncoding did not produce an object for:" <> (show $ typeRep $ (Proxy :: Proxy a)))

-- | "kind": { "type": "string", "enum": [ "reveal" ] },
data OperationContentsReveal = OperationContentsReveal
  { _operationContentsReveal_metadata :: ManagerOperationMetadata OperationResultReveal
  , _operationContentsReveal_source :: !ContractId -- ^ "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsReveal_fee :: !Tez -- ^ "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsReveal_counter :: !Integer-- ^ "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_gasLimit :: !Integer -- ^ "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_storageLimit :: !Integer -- ^ "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_publicKey :: !PublicKey -- ^ "public_key": { "$ref": "#/definitions/Signature.Public_key" },
  }
  deriving (Eq, Ord, Show, Typeable)


data OperationResultReveal = OperationResultReveal
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "transaction" ] },
data OperationContentsTransaction = OperationContentsTransaction
  { _operationContentsTransaction_metadata :: ManagerOperationMetadata OperationResultTransaction
  , _operationContentsTransaction_source :: !ContractId -- ^ "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsTransaction_fee :: !Tez -- ^ "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsTransaction_counter :: !Integer-- ^ "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_gasLimit :: !Integer -- ^ "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_storageLimit :: !Integer -- ^ "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_amount :: !Tez -- ^ "amount": { "$ref": "#/definitions/mutez" },
  , _operationContentsTransaction_destination :: !ContractId -- ^ "destination": { "$ref": "#/definitions/contract_id" },
  , _operationContentsTransaction_parameters :: !Expression -- ^ "parameters": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  }
  deriving (Eq, Ord, Show, Typeable)


-- | "operation.alpha.operation_result.transaction": {
data OperationResultTransaction = OperationResultTransaction
  { _operationResultTransaction_storage :: !(Maybe Expression) -- ^ "storage": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  , _operationResultTransaction_balanceUpdates :: !(Maybe (Seq BalanceUpdate)) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
  , _operationResultTransaction_originatedContracts :: !(Maybe (Seq ContractId)) -- ^ "originated_contracts": { "type": "array", "items": { "$ref": "#/definitions/contract_id" } },
  , _operationResultTransaction_consumedGas :: !(Maybe Integer) -- ^ "consumed_gas": { "$ref": "#/definitions/bignum" },
  , _operationResultTransaction_storageSize :: !(Maybe Integer) -- ^ "storage_size": { "$ref": "#/definitions/bignum" },
  , _operationResultTransaction_paidStorageSizeDiff :: !(Maybe Integer) -- ^ "paid_storage_size_diff": { "$ref": "#/definitions/bignum" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "origination" ] },
data OperationContentsOrigination = OperationContentsOrigination
  { _operationContentsOrigination_metadata :: ManagerOperationMetadata OperationResultOrigination
  , _operationContentsOrigination_source :: !ContractId -- ^ "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsOrigination_fee :: !Tez -- ^ "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsOrigination_counter :: !Integer-- ^ "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_gasLimit :: !Integer -- ^ "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_storageLimit :: !Integer -- ^ "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_managerPubkey :: !PublicKeyHash -- ^ "managerPubkey": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsOrigination_balance :: !Tez -- ^ "balance": { "$ref": "#/definitions/mutez" },
  , _operationContentsOrigination_spendable :: !Bool -- ^ "spendable": { "type": "boolean" },
  , _operationContentsOrigination_delegatable :: !Bool -- ^ "delegatable": { "type": "boolean" },
  , _operationContentsOrigination_delegate :: !PublicKeyHash -- ^ "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsOrigination_script :: !ContractScript -- ^ "script": { "$ref": "#/definitions/scripted.contracts" },
  }
  deriving (Eq, Ord, Show, Typeable)

data OperationResultOrigination = OperationResultOrigination
  { _operationResultOrigination_balanceUpdates :: !(Maybe (Seq BalanceUpdate)) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
  , _operationResultOrigination_originatedContracts :: !(Maybe (Seq ContractId)) -- ^ "originated_contracts": { "type": "array", "items": { "$ref": "#/definitions/contract_id" } },
  , _operationResultOrigination_consumedGas :: !(Maybe Integer) -- ^ "consumed_gas": { "$ref": "#/definitions/bignum" },
  , _operationResultOrigination_storageSize :: !(Maybe Integer) -- ^ "storage_size": { "$ref": "#/definitions/bignum" },
  , _operationResultOrigination_paidStorageSizeDiff :: !(Maybe Integer) -- ^ "paid_storage_size_diff": { "$ref": "#/definitions/bignum" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "delegation" ] },
data OperationContentsDelegation = OperationContentsDelegation
  { _operationContentsDelegation_metadata :: ManagerOperationMetadata OperationResultDelegation
  , _operationContentsDelegation_source :: !ContractId -- ^ "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsDelegation_fee :: !Tez -- ^ "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsDelegation_counter :: !Integer-- ^ "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_gasLimit :: !Integer -- ^ "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_storageLimit :: !Integer -- ^ "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_delegate :: !PublicKeyHash -- ^ "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  }
  deriving (Eq, Ord, Show, Typeable)

data OperationResultDelegation = OperationResultDelegation
  deriving (Eq, Ord, Show, Typeable)

concat <$> traverse deriveTezosJson
  [ ''Operation
  , ''OperationContentsEndorsement , ''EndorsementMetadata
  , ''OperationContentsSeedNonceRevelation , ''SeedNonceRevelationMetadata
  , ''OperationContentsDoubleEndorsementEvidence , ''DoubleEndorsementEvidenceMetadata
  , ''InlinedEndorsement , ''InlinedEndorsementContents
  , ''OperationContentsDoubleBakingEvidence , ''DoubleBakingEvidenceMetadata
  , ''OperationContentsActivateAccount , ''ActivateMetadata
  , ''OperationContentsProposals
  , ''OperationContentsBallot , ''Ballot
  , ''OperationResultStatus
  , ''OperationContentsReveal , ''OperationResultReveal
  , ''OperationContentsTransaction , ''OperationResultTransaction
  , ''OperationContentsOrigination , ''OperationResultOrigination
  , ''OperationContentsDelegation, ''OperationResultDelegation
  ]


instance (ToJSON a, Typeable a) => ToJSON (ManagerOperationMetadata a) where
  toJSON = $(Aeson.mkToJSON tezosJsonOptions ''ManagerOperationMetadata)
  toEncoding = $(Aeson.mkToEncoding tezosJsonOptions ''ManagerOperationMetadata)

instance (FromJSON a, Typeable a) => FromJSON (ManagerOperationMetadata a) where
  parseJSON = $(Aeson.mkParseJSON tezosJsonOptions ''ManagerOperationMetadata)
