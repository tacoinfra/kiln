{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Tezos.Operation where

import Control.Lens ((<&>),Traversal')
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Applicative ((<|>))
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Binary.Get (bytesRead)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Dependent.Sum (DSum(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Some (Some(..), withSome)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import GHC.Word
import qualified Data.Aeson.TH as Aeson
import qualified Data.HashMap.Strict as HashMap
import Numeric.Natural (Natural)

import Tezos.BalanceUpdate
import Tezos.Base16ByteString
import Tezos.Base58Check
import qualified Tezos.Binary as B
import Tezos.Binary ((<**))
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
  { _operation_protocol :: !ProtocolHash --          "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _operation_chainId :: !ChainId --          "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _operation_hash :: !OperationHash --          "hash": { "$ref": "#/definitions/Operation_hash" },
  , _operation_branch :: !BlockHash --          "branch": { "$ref": "#/definitions/block_hash" },
  , _operation_contents :: !(Seq OperationContents) --          "contents": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.operation_contents_and_result" } },
                                                 --          "contents": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.contents" } },
  , _operation_signature :: !(Maybe Signature) --          "signature": { "$ref": "#/definitions/Signature" }
  }
  deriving (Eq, Ord, Show, Typeable)

data OpKind
  = OpKind_SeedNonceRevelation
  | OpKind_DoubleEndorsementEvidence
  | OpKind_DoubleBakingEvidence
  | OpKind_ActivateAccount
  | OpKind_Endorsement
  | OpKind_Proposals
  | OpKind_Ballot
  | OpKind_Manager [OpKindManager]

data OpKindTag opKind where
  -- OpKindTag_SeedNonceRevelation :: OpKindTag OpKind_SeedNonceRevelation
  -- OpKindTag_DoubleEndorsementEvidence :: OpKindTag OpKind_DoubleEndorsementEvidence
  -- OpKindTag_DoubleBakingEvidence :: OpKindTag OpKind_DoubleBakingEvidence
  -- OpKindTag_ActivateAccount :: OpKindTag OpKind_ActivateAccount
  OpKindTag_Endorsement :: OpKindTag 'OpKind_Endorsement
  -- OpKindTag_Proposals :: OpKindTag OpKind_Proposals
  -- OpKindTag_Ballot :: OpKindTag OpKind_Ballot
  -- OpKindTag_Reveal :: OpKindTag OpKind_Reveal
  -- OpKindTag_Origination :: OpKindTag OpKind_Origination
  -- OpKindTag_Delegation :: OpKindTag OpKind_Delegation
  OpKindTag_Manager :: OpKindManagerTag opKindManager -> OpKindTag ('OpKind_Manager (opKindManager : '[]))
  deriving (Typeable)

data OpsKindTag opKinds where
  OpsKindTag_Single :: OpKindTag k -> OpsKindTag k
  -- OpsKindTag_Cons :: OpKindTag (OpKind_Manager (k : '[])) -> OpsKindTag (OpKind_Manager ks) -> OpsKindTag (OpKind_Manager (k : ks))
  -- TODO support operation batching since this attempt did not work
  deriving (Typeable)

data OpKindManager
  = OpKindManager_Reveal
  | OpKindManager_Transaction
  | OpKindManager_Origination
  | OpKindManager_Delegation
  deriving (Typeable)

data OpKindManagerTag opKindManager where
  -- OpKindManagerTag_Reveal :: OpKindManagerTag OpKindManager_Reveal
  OpKindManagerTag_Transaction :: OpKindManagerTag 'OpKindManager_Transaction
  -- OpKindManagerTag_Origination :: OpKindManagerTag OpKindManager_Origination
  -- OpKindManagerTag_Delegation :: OpKindManagerTag OpKindManager_Delegation
  deriving (Typeable)

data Op (a :: OpKind) = Op
  { _op_branch :: !BlockHash
  , _op_contents :: !(OpContentsList a)
  , _op_signature :: !(Maybe Signature)
  }
  deriving (Eq, Ord, Show, Typeable)

data EmptyMetadata = EmptyMetadata
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON EmptyMetadata where
  parseJSON = withObject "EmptyMetadata" $ const $ pure EmptyMetadata

instance ToJSON EmptyMetadata where
  toJSON _ = Object $ mempty
  toEncoding _ = pairs mempty


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

data OpContentsList (a :: OpKind) where
  OpContentsList_Single :: OpContents a -> OpContentsList a
  -- OpContentsList_Cons :: OpContents ('OpKind_Manager '[a]) -> OpContentsList ('OpKind_Manager as) -> OpContentsList ('OpKind_Manager (a : as))
  deriving Typeable

deriving instance Eq (OpContentsList a)
deriving instance Ord (OpContentsList a)
deriving instance Show (OpContentsList a)

data OpContents (a :: OpKind) where
  OpContents_Endorsement :: !OpContentsEndorsement -> OpContents 'OpKind_Endorsement
  OpContents_Transaction :: !(OpContentsManager OpContentsTransaction) -> OpContents ('OpKind_Manager '[ 'OpKindManager_Transaction])
  deriving Typeable

deriving instance Eq (OpContents a)
deriving instance Ord (OpContents a)
deriving instance Show (OpContents a)

data OpContentsEndorsement = OpContentsEndorsement
  { _opContentsEndorsement_level :: !RawLevel
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsManager op = OpContentsManager
  { _opContentsManager_source :: !ContractId
  , _opContentsManager_fee :: !Tez
  , _opContentsManager_counter :: !Natural
  , _opContentsManager_gasLimit :: !Natural
  , _opContentsManager_storageLimit :: !Natural
  , _opContentsManager_operation :: !op
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsTransaction = OpContentsTransaction
  { _opContentsTransaction_amount :: !Tez
  , _opContentsTransaction_destination :: !ContractId
  , _opContentsTransaction_parameters :: !(Maybe Expression)
  }
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
  , _operationContentsEndorsement_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  }
  deriving (Eq, Ord, Show, Typeable)

data EndorsementMetadata = EndorsementMetadata
  { _endorsementMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
  , _endorsementMetadata_delegate :: !PublicKeyHash--  "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _endorsementMetadata_slots :: !(Seq Word8) --  "slots": { "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 255 } }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "seed_nonce_revelation" ] },
data OperationContentsSeedNonceRevelation = OperationContentsSeedNonceRevelation
  { _operationContentsSeedNonceRevelation_metadata :: SeedNonceRevelationMetadata
  , _operationContentsSeedNonceRevelation_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsSeedNonceRevelation_nonce :: !(Base16ByteString ByteString) --  "nonce": { "type": "string", "pattern": "^[a-zA-Z0-9]+$" },
  }
  deriving (Eq, Ord, Show, Typeable)

data SeedNonceRevelationMetadata = SeedNonceRevelationMetadata
  { _seedNonceRevelationMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

data InlinedEndorsement = InlinedEndorsement
  { _inlinedEndorsement_branch :: !BlockHash--  "branch": { "$ref": "#/definitions/block_hash" },
  , _inlinedEndorsement_operations :: !InlinedEndorsementContents --  "operations": { "$ref": "#/definitions/inlined.endorsement.contents" },
  , _inlinedEndorsement_signature :: !(Maybe Signature) --  "signature": { "$ref": "#/definitions/Signature" }
  }
  deriving (Eq, Ord, Show, Typeable)

data InlinedEndorsementContents = InlinedEndorsementContents
  { _inlinedEndorsementContents_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } },
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_endorsement_evidence" ] },
data OperationContentsDoubleEndorsementEvidence = OperationContentsDoubleEndorsementEvidence
  { _operationContentsDoubleEndorsementEvidence_metadata :: DoubleEndorsementEvidenceMetadata
  , _operationContentsDoubleEndorsementEvidence_op1 :: InlinedEndorsement --  "op1": { "$ref": "#/definitions/inlined.endorsement" },
  , _operationContentsDoubleEndorsementEvidence_op2 :: InlinedEndorsement --  "op2": { "$ref": "#/definitions/inlined.endorsement" },
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleEndorsementEvidenceMetadata = DoubleEndorsementEvidenceMetadata
  { _doubleEndorsementEvidenceMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "double_baking_evidence" ] },
data OperationContentsDoubleBakingEvidence = OperationContentsDoubleBakingEvidence
  { _operationContentsDoubleBakingEvidence_metadata :: !DoubleBakingEvidenceMetadata
  , _operationContentsDoubleBakingEvidence_bh1 :: !BlockHeader --  "bh1": { "$ref": "#/definitions/block_header.alpha.full_header" },
  , _operationContentsDoubleBakingEvidence_bh2 :: !BlockHeader --  "bh2": { "$ref": "#/definitions/block_header.alpha.full_header" },
  }
  deriving (Eq, Ord, Show, Typeable)

data DoubleBakingEvidenceMetadata = DoubleBakingEvidenceMetadata
  { _doubleBakingEvidenceMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "activate_account" ] },
data OperationContentsActivateAccount = OperationContentsActivateAccount
  { _operationContentsActivateAccount_metadata :: !ActivateMetadata
  , _operationContentsActivateAccount_pkh :: !Ed25519PublicKeyHash--  "pkh": { "$ref": "#/definitions/Ed25519.Public_key_hash" },
  , _operationContentsActivateAccount_secret :: !(Base16ByteString ByteString) --  "secret": { "type": "string", "pattern": "^[a-zA-Z0-9]+$" },
  }
  deriving (Eq, Ord, Show, Typeable)

data ActivateMetadata = ActivateMetadata
  { _activateMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "proposals" ] },
data OperationContentsProposals = OperationContentsProposals
  { _operationContentsProposals_metadata :: !EmptyMetadata --  "metadata": { "type": "object", "properties": {}, "additionalProperties": false }
  , _operationContentsProposals_source :: !PublicKeyHash --  "source": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsProposals_period :: !RawLevel --  "period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsProposals_proposals :: !(Seq ProtocolHash) --  "proposals": { "type": "array", "items": { "$ref": "#/definitions/Protocol_hash" } },
  }
  deriving (Eq, Ord, Show, Typeable)

-- | "ballot": { "type": "string", "enum": [ "nay", "yay", "pass" ] },
data Ballot
   = Ballot_Nay
   | Ballot_Yay
   | Ballot_Pass
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Generic)

-- | "kind": { "type": "string", "enum": [ "ballot" ] },
data OperationContentsBallot = OperationContentsBallot
  { _operationContentsBallot_metadata :: !EmptyMetadata --  "metadata": { "type": "object", "properties": {}, "additionalProperties": false }
  , _operationContentsBallot_source :: !PublicKeyHash --  "source": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsBallot_period :: !RawLevel --  "period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _operationContentsBallot_proposal :: !ProtocolHash --  "proposal": { "$ref": "#/definitions/Protocol_hash" },
  , _operationContentsBallot_ballot :: !Ballot
  }
  deriving (Eq, Ord, Show, Typeable)

data ManagerOperationMetadata a = ManagerOperationMetadata
  { _managerOperationMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  , _managerOperationMetadata_operationResult :: !(OperationResult a) --  "operation_result": { "$ref": "#/definitions/operation.alpha.operation_result.reveal" },
  -- I don't see these in the output from the nodes, seems redundant,  i'll skip them for now.
  -- , _managerOperationMetadata_internalOperationResults :: !(Seq InternalOperationResult) --  "internal_operation_results": { "type": "array", "items": { "$ref": "#/definitions/operation.alpha.internal_operation_result" } }
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:500:           (dft "internal_operation_results"
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:501:              (list internal_operation_result_encoding) [])) ;
  }
  deriving (Eq, Ord, Show, Typeable)

data OperationResultStatus
  =  OperationResultStatus_Applied     --  no errors, have result
  |  OperationResultStatus_Failed      --  have errors no result
  |  OperationResultStatus_Skipped     --  no errors, no result
  |  OperationResultStatus_Backtracked --  errors and result
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
  parseJSON = withObject (show $ typeRep (Proxy :: Proxy a)) $ \v -> OperationResult
    <$> v .: "status"
    <*> v .:? "errors"
    <*> (Just <$> parseJSON (Object v) <|> pure Nothing)
        -- let x = OperationResult status <$> case status of
        --       OperationResultStatus_Applied -> pure Nothing
        --       OperationResultStatus_Failed -> v .:? "errors"
        --       OperationResultStatus_Skipped -> pure Nothing
        --       OperationResultStatus_Backtracked -> v .:? "errors"
        -- x <*> case status of
        --   OperationResultStatus_Applied -> Just <$> parseJSON (Object v)
        --   OperationResultStatus_Failed -> pure Nothing
        --   OperationResultStatus_Skipped -> pure Nothing
        --   OperationResultStatus_Backtracked -> parseJSON (Object v)

instance (Typeable a, ToJSON a) => ToJSON (OperationResult a) where
  -- toJSON :: forall a. (ToJSON a, Typeable a) => OperationResult a -> Value
  toJSON x = Object (status <> errors <> content)
    where
      status = HashMap.singleton "status" (toJSON $ _operationResult_status x)
      errors = foldMap (HashMap.singleton "errors" . toJSON) $ _operationResult_errors x
      content = case toJSON <$> _operationResult_content x of
          Nothing -> mempty
          Just (Object x') -> x'
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
  , _operationContentsReveal_source :: !ContractId --  "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsReveal_fee :: !Tez --  "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsReveal_counter :: !TezosWord64--  "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_gasLimit :: !TezosWord64 --  "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_storageLimit :: !TezosWord64 --  "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsReveal_publicKey :: !PublicKey --  "public_key": { "$ref": "#/definitions/Signature.Public_key" },
  }
  deriving (Eq, Ord, Show, Typeable)


data OperationResultReveal = OperationResultReveal
  deriving (Eq, Ord, Show, Typeable)

-- | "kind": { "type": "string", "enum": [ "transaction" ] },
data OperationContentsTransaction = OperationContentsTransaction
  { _operationContentsTransaction_metadata :: ManagerOperationMetadata OperationResultTransaction
  , _operationContentsTransaction_source :: !ContractId --  "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsTransaction_fee :: !Tez --  "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsTransaction_counter :: !TezosWord64--  "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_gasLimit :: !TezosWord64 --  "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_storageLimit :: !TezosWord64 --  "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsTransaction_amount :: !Tez --  "amount": { "$ref": "#/definitions/mutez" },
  , _operationContentsTransaction_destination :: !ContractId --  "destination": { "$ref": "#/definitions/contract_id" },
  , _operationContentsTransaction_parameters :: !(Maybe Expression) --  "parameters": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  }
  deriving (Eq, Ord, Show, Typeable)


-- | "operation.alpha.operation_result.transaction": {
data OperationResultTransaction = OperationResultTransaction
  { _operationResultTransaction_storage :: !(Maybe Expression) --  "storage": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  , _operationResultTransaction_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:170:           (dft "balance_updates" Delegate.balance_updates_encoding [])
  , _operationResultTransaction_originatedContracts :: !(Seq ContractId) --  "originated_contracts": { "type": "array", "items": { "$ref": "#/definitions/contract_id" } },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:171:           (dft "originated_contracts" (list Contract.encoding) [])
  , _operationResultTransaction_consumedGas :: !TezosWord64 --  "consumed_gas": { "$ref": "#/definitions/bignum" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:172:           (dft "consumed_gas" z Z.zero)
  , _operationResultTransaction_storageSize :: !TezosWord64 --  "storage_size": { "$ref": "#/definitions/bignum" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:173:           (dft "storage_size" z Z.zero)
  , _operationResultTransaction_paidStorageSizeDiff :: !TezosWord64 --  "paid_storage_size_diff": { "$ref": "#/definitions/bignum" }
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:174:           (dft "paid_storage_size_diff" z Z.zero))
  }
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON OperationResultTransaction where
  parseJSON = withObject "OperationResultTransaction" $ \v -> OperationResultTransaction
    <$> v .: "storage"
    <*> v .:? "balance_updates" .!= mempty
    <*> v .:? "originated_contracts" .!= mempty
    <*> v .:? "consumed_gas" .!= 0
    <*> v .:? "storage_size" .!= 0
    <*> v .:? "paid_storage_size_diff" .!= 0


-- | "kind": { "type": "string", "enum": [ "origination" ] },
data OperationContentsOrigination = OperationContentsOrigination
  { _operationContentsOrigination_metadata :: ManagerOperationMetadata OperationResultOrigination
  , _operationContentsOrigination_source :: !ContractId --  "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsOrigination_fee :: !Tez --  "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsOrigination_counter :: !TezosWord64--  "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_gasLimit :: !TezosWord64 --  "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_storageLimit :: !TezosWord64 --  "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsOrigination_managerPubkey :: !PublicKeyHash --  "managerPubkey": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsOrigination_balance :: !Tez --  "balance": { "$ref": "#/definitions/mutez" },
  , _operationContentsOrigination_spendable :: !Bool --  "spendable": { "type": "boolean" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/operation_repr.ml:258:             (dft "spendable" bool true)
  , _operationContentsOrigination_delegatable :: !Bool --  "delegatable": { "type": "boolean" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/operation_repr.ml:259:             (dft "delegatable" bool true)
  , _operationContentsOrigination_delegate :: !(Maybe PublicKeyHash) --  "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsOrigination_script :: !(Maybe ContractScript) --  "script": { "$ref": "#/definitions/scripted.contracts" },
  }
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON OperationContentsOrigination where
  parseJSON = withObject "OperationContentsOrigination" $ \v -> OperationContentsOrigination
    <$> v .: "metadata"
    <*> v .: "source"
    <*> v .: "fee"
    <*> v .: "counter"
    <*> v .: "gas_limit"
    <*> v .: "storage_limit"
    -- We need this hand written instance due to
    -- https://gitlab.com/tezos/tezos/issues/276
    -- Once that's resolved, we can go back to deriving this as usual
    <*> (v .: "manager_pubkey" <|> v .: "managerPubkey")
    <*> v .: "balance"
    <*> v .:? "spendable" .!= True
    <*> v .:? "delegatable" .!= True
    <*> v .:? "delegate"
    <*> v .:? "script"

data OperationResultOrigination = OperationResultOrigination
  { _operationResultOrigination_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:208:           (dft "balance_updates" Delegate.balance_updates_encoding [])
  , _operationResultOrigination_originatedContracts :: !(Seq ContractId) --  "originated_contracts": { "type": "array", "items": { "$ref": "#/definitions/contract_id" } },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:209:           (dft "originated_contracts" (list Contract.encoding) [])
  , _operationResultOrigination_consumedGas :: !TezosWord64 --  "consumed_gas": { "$ref": "#/definitions/bignum" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:210:           (dft "consumed_gas" z Z.zero)
  , _operationResultOrigination_storageSize :: !TezosWord64 --  "storage_size": { "$ref": "#/definitions/bignum" },
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:211:           (dft "storage_size" z Z.zero)
  , _operationResultOrigination_paidStorageSizeDiff :: !TezosWord64 --  "paid_storage_size_diff": { "$ref": "#/definitions/bignum" }
-- src/proto_002_PsYLVpVv/lib_protocol/src/apply_results.ml:212:           (dft "paid_storage_size_diff" z Z.zero))
  }
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON OperationResultOrigination where
  parseJSON = withObject "OperationResultOrigination" $ \v -> OperationResultOrigination
    <$> v .:? "balance_updates" .!= mempty
    <*> v .:? "originated_contracts" .!= mempty
    <*> v .:? "consumed_gas" .!= 0
    <*> v .:? "storage_size" .!= 0
    <*> v .:? "paid_storage_size_diff" .!= 0


-- | "kind": { "type": "string", "enum": [ "delegation" ] },
data OperationContentsDelegation = OperationContentsDelegation
  { _operationContentsDelegation_metadata :: ManagerOperationMetadata OperationResultDelegation
  , _operationContentsDelegation_source :: !ContractId --  "source": { "$ref": "#/definitions/contract_id" },
  , _operationContentsDelegation_fee :: !Tez --  "fee": { "$ref": "#/definitions/mutez" },
  , _operationContentsDelegation_counter :: !TezosWord64--  "counter": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_gasLimit :: !TezosWord64 --  "gas_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_storageLimit :: !TezosWord64 --  "storage_limit": { "$ref": "#/definitions/positive_bignum" },
  , _operationContentsDelegation_delegate :: !(Maybe PublicKeyHash) --  "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  }
  deriving (Eq, Ord, Show, Typeable)

data OperationResultDelegation = OperationResultDelegation
  deriving (Eq, Ord, Show, Typeable)

stripEndorsement :: Operation -> Maybe (Op 'OpKind_Endorsement)
stripEndorsement (Operation { _operation_branch = branch, _operation_contents = contents, _operation_signature = sig })
  | length contents /= 1 = Nothing
  | otherwise = case Seq.index contents 0 of
      OperationContents_Endorsement (OperationContentsEndorsement { _operationContentsEndorsement_level = level }) ->
        Just $ Op { _op_branch = branch, _op_contents = OpContentsList_Single $ OpContents_Endorsement $ OpContentsEndorsement level, _op_signature = sig }
      _ -> Nothing

outlineEndorsement :: InlinedEndorsement -> Op 'OpKind_Endorsement
outlineEndorsement (InlinedEndorsement { _inlinedEndorsement_branch = branch, _inlinedEndorsement_operations = contents, _inlinedEndorsement_signature = sig })
  = case contents of
      InlinedEndorsementContents { _inlinedEndorsementContents_level = level } ->
        Op { _op_branch = branch, _op_contents = OpContentsList_Single $ OpContents_Endorsement $ OpContentsEndorsement level, _op_signature = sig }

instance B.TezosBinary (Some OpKindTag) where
  put t = withSome t $ \case
    OpKindTag_Endorsement -> B.put @Word8 0
    OpKindTag_Manager OpKindManagerTag_Transaction -> B.put @Word8 8
  get = B.get @Word8 >>= \case
    0 -> pure $ This OpKindTag_Endorsement
    8 -> pure $ This (OpKindTag_Manager OpKindManagerTag_Transaction)
    _ -> fail "currently unsupported operation tag"

instance B.TezosBinary OpContentsEndorsement where
  put = B.puts _opContentsEndorsement_level
  get = OpContentsEndorsement <$> B.get

instance B.TezosBinary OpContentsTransaction where
  put = B.puts _opContentsTransaction_amount
    <** B.puts _opContentsTransaction_destination
    <** B.puts _opContentsTransaction_parameters
  get = pure OpContentsTransaction
    <*> B.get
    <*> B.get
    <*> B.get

newtype TenByteNatural = TenByteNatural { unTenByteNatural :: Natural }
  deriving (Eq, Ord, Show, Typeable)

instance B.TezosBinary TenByteNatural where
  build (TenByteNatural n) =
    let x = B.build n in
      if BSL.null $ BSL.drop 10 $ toLazyByteString x then x else error "number too big (>10 bytes)"
  get = do
    start <- bytesRead
    n <- B.get @Natural
    end <- bytesRead
    if end - 10 > start then fail "number too big (>10 bytes)"
      else return $ TenByteNatural n

instance B.TezosBinary op => B.TezosBinary (OpContentsManager op) where
  put = B.puts _opContentsManager_source
    <** B.puts _opContentsManager_fee
    <** B.puts (TenByteNatural . _opContentsManager_counter)
    <** B.puts (TenByteNatural . _opContentsManager_gasLimit)
    <** B.puts (TenByteNatural . _opContentsManager_storageLimit)
    <** B.puts _opContentsManager_operation
  get = pure OpContentsManager
    <*> B.get
    <*> B.get
    <*> fmap unTenByteNatural B.get
    <*> fmap unTenByteNatural B.get
    <*> fmap unTenByteNatural B.get
    <*> B.get

instance B.TezosBinary (OpContents 'OpKind_Endorsement) where
  put = \case
    OpContents_Endorsement op -> B.put @Word8 0 <* B.put op
  get = B.get @Word8 >>= \case
    0 -> OpContents_Endorsement <$> B.get
    _ -> fail "not an endorsement"

instance B.TezosBinary (DSum OpKindTag OpContents) where
  put (t :=> op) = B.put (This t) *>
    case op of
      OpContents_Endorsement opc -> B.put opc
      OpContents_Transaction opc -> B.put opc
  get = B.get >>= \case
    This t -> (t :=>) <$> case t of
      OpKindTag_Endorsement -> OpContents_Endorsement <$> B.get
      OpKindTag_Manager OpKindManagerTag_Transaction -> OpContents_Transaction <$> B.get

instance B.TezosBinary (OpContentsList 'OpKind_Endorsement) where
  put = \case
    OpContentsList_Single op -> B.put op
  get = OpContentsList_Single <$> B.get

instance B.TezosBinary (DSum OpsKindTag OpContentsList) where
  put = \case
    OpsKindTag_Single t :=> OpContentsList_Single op ->
      B.put $ t :=> op
    -- OpsKindTag_Cons t ts :=> OpContentsList_Cons op ops -> do
    --   B.put $ t :=> op
    --   B.put $ ts :=> ops
  get = B.get <&> \case
    t :=> op -> OpsKindTag_Single t :=> OpContentsList_Single op

instance B.TezosUnsignedBinary (Op 'OpKind_Endorsement) where
  putUnsigned = shellHeaderEncoding <** B.puts _op_contents
    where
      shellHeaderEncoding = B.puts _op_branch
  getUnsigned = shellHeaderDecoding <*> B.get <*> pure Nothing
    where
      shellHeaderDecoding = Op <$> B.get

instance B.TezosUnsignedBinary (DSum OpsKindTag Op) where
  putUnsigned (tag :=> op) =
      shellHeaderEncoding op *>
      B.puts ((tag :=>) . _op_contents) op
    where
      shellHeaderEncoding = B.puts _op_branch
  getUnsigned = shellHeaderDecoding <*> B.get <*> pure Nothing
    where
      shellHeaderDecoding = do
        shellHeader <- B.get
        return $ \case
          (tag :=> protocolData) -> (tag :=>) . Op shellHeader protocolData

instance B.TezosBinary (DSum OpsKindTag Op) where
  put op@(_ :=> body) =
      B.putUnsigned op *> B.puts _op_signature body
  get = B.getUnsigned >>= \case
    (t :=> op) -> do
      sig <- B.get
      pure $ t :=> op { _op_signature = Just sig }

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
  , ''OperationContentsTransaction
  , ''OperationContentsDelegation, ''OperationResultDelegation
  ]


instance (ToJSON a, Typeable a) => ToJSON (ManagerOperationMetadata a) where
  toJSON = $(Aeson.mkToJSON tezosJsonOptions ''ManagerOperationMetadata)
  toEncoding = $(Aeson.mkToEncoding tezosJsonOptions ''ManagerOperationMetadata)

instance (FromJSON a, Typeable a) => FromJSON (ManagerOperationMetadata a) where
  parseJSON = $(Aeson.mkParseJSON tezosJsonOptions ''ManagerOperationMetadata)

fmap concat $ sequence
  [ concat <$> traverse (Aeson.deriveToJSON tezosJsonOptions)
    [ ''OperationContentsOrigination
    , ''OperationResultOrigination
    , ''OperationResultTransaction
    ]
  , concat <$> traverse makePrisms
    [ ''OperationContents
    ]
  , concat <$> traverse makeLenses
    [ 'Operation
    , 'ActivateMetadata
    , 'DoubleBakingEvidenceMetadata
    , 'DoubleEndorsementEvidenceMetadata
    , 'EndorsementMetadata
    , 'InlinedEndorsement
    , 'InlinedEndorsementContents
    , 'ManagerOperationMetadata
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
    , 'SeedNonceRevelationMetadata
    ]
  ]

instance HasBalanceUpdates Operation where
  -- balanceUpdates :: Traversal' Operation BalanceUpdate
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
-- src/proto_002_PsYLVpVv/lib_protocol/src/helpers_services.ml:358:             (dft "proof_of_work_nonce"
