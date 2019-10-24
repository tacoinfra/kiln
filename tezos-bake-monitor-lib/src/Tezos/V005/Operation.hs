{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Tezos.V005.Operation where

import Control.Lens (Traversal')
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Applicative ((<|>))
import Control.Monad ((<=<), replicateM, when)
import Control.Monad.Fail (MonadFail, fail)
import Data.Aeson
import Data.Binary.Get (isolate)
import Data.Char (isLatin1)
import Data.Function ((&))
import Data.Functor.Compose (Compose(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (traverse_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Some (Some(..), withSome)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import GHC.Word
import qualified Data.Aeson.TH as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Prelude hiding (fail)

import Tezos.Common.Base16ByteString
import Tezos.Common.Base58Check
import Tezos.Common.Json
import Tezos.V005.BalanceUpdate
import qualified Tezos.Common.Binary as B
import Tezos.Common.Binary ((<**))
import Tezos.V005.BlockHeader
import Tezos.V005.Contract
import Tezos.V005.Level
import Tezos.V005.Micheline
import Tezos.V005.PublicKey
import Tezos.V005.PublicKeyHash
import Tezos.V005.Signature
import Tezos.V005.Tez

{-
Changes from V005 to V004:
- depends on the new contract script that has the new micheline primitive
- Script is non optional on originations now
- spendable, manager_pubKey and delegatable dissappear on originations
- Scripts now have entrypoints, so parameters are accompanied by entrypoints

See: https://tezos.gitlab.io/master/protocols/005_babylon.html#id4 for more details

Stuff on the Michelson Entrypoints is explained by https://blog.nomadic-labs.com/michelson-updates-in-005.html
-}

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
  OpKindTag_Endorsement :: OpKindTag 'OpKind_Endorsement
  OpKindTag_SeedNonceRevelation :: OpKindTag 'OpKind_SeedNonceRevelation
  OpKindTag_DoubleEndorsementEvidence :: OpKindTag 'OpKind_DoubleEndorsementEvidence
  OpKindTag_DoubleBakingEvidence :: OpKindTag 'OpKind_DoubleBakingEvidence
  OpKindTag_ActivateAccount :: OpKindTag 'OpKind_ActivateAccount
  OpKindTag_Proposals :: OpKindTag 'OpKind_Proposals
  OpKindTag_Ballot :: OpKindTag 'OpKind_Ballot
  OpKindTag_Manager :: OpKindManagerTag opKindManager -> OpKindTag ('OpKind_Manager (opKindManager : '[]))
  deriving (Typeable)

data OpsKindTag opKinds where
  OpsKindTag_Single :: OpKindTag k -> OpsKindTag k
  OpsKindTag_Cons :: OpKindTag ('OpKind_Manager (k ': '[])) -> OpsKindTag ('OpKind_Manager (kk ': ks)) -> OpsKindTag ('OpKind_Manager (k ': kk ': ks))
  -- TODO support operation batching since this attempt did not work
  deriving (Typeable)

data OpKindManager
  = OpKindManager_Reveal
  | OpKindManager_Transaction
  | OpKindManager_Origination
  | OpKindManager_Delegation
  deriving (Typeable)

data OpKindManagerTag opKindManager where
  OpKindManagerTag_Reveal :: OpKindManagerTag 'OpKindManager_Reveal
  OpKindManagerTag_Transaction :: OpKindManagerTag 'OpKindManager_Transaction
  OpKindManagerTag_Origination :: OpKindManagerTag 'OpKindManager_Origination
  OpKindManagerTag_Delegation :: OpKindManagerTag 'OpKindManager_Delegation
  deriving (Typeable)

data BatchableOp k where
  BatchableOp :: OpKindManagerTag k -> BatchableOp ('OpKind_Manager '[k])

data Op (a :: OpKind) = Op
  { _op_branch :: !BlockHash
  , _op_contents :: !(OpContentsList a)
  , _op_signature :: !(Maybe Signature)
  }
  deriving (Eq, Ord, Show, Typeable)

-- | next_operation:
-- { "protocol": "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd",
--   "branch": $block_hash,
--   "contents": [ $operation.alpha.contents ... ],
--   "signature": $Signature }
--
-- Like Op but with a protocol hash added. Used in preapply.
data ProtoOp (a :: OpKind) = ProtoOp
  { _protoOp_protocol :: !ProtocolHash
  , _protoOp_branch :: !BlockHash
  , _protoOp_contents :: !(OpContentsList a)
  , _protoOp_signature :: !(Maybe Signature)
  } deriving (Eq, Ord, Show, Typeable)

stripOpProtocol :: ProtoOp a -> Op a
stripOpProtocol ProtoOp { _protoOp_protocol, _protoOp_branch, _protoOp_contents, _protoOp_signature }
  = Op _protoOp_branch _protoOp_contents _protoOp_signature

-- | Like Op but with its own hash. Used in the mempool.
data PendingOp (a :: OpKind) = PendingOp
  { _pendingOp_hash :: !OperationHash
  , _pendingOp_branch :: !BlockHash
  , _pendingOp_contents :: !(OpContentsList a)
  , _pendingOp_signature :: !Signature
  } deriving (Eq, Ord, Show, Typeable)

-- | This is also used in the mempool for operations that will not be
-- applied for one reason or another.
data ErroredOp (a :: OpKind) = ErroredOp
  { _erroredOp_protocol :: !ProtocolHash
  , _erroredOp_branch :: !BlockHash
  , _erroredOp_contents :: !(OpContentsList a)
  , _erroredOp_signature :: !Signature
  , _erroredOp_error :: !Value
  } deriving (Eq, Show, Typeable)

data EmptyMetadata = EmptyMetadata
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON EmptyMetadata where
  parseJSON = withObject "EmptyMetadata" $ const $ pure EmptyMetadata

instance ToJSON EmptyMetadata where
  toJSON _ = Object mempty
  toEncoding _ = pairs mempty

-- | operation.alpha.operation_with_metadata
-- { "contents": [ $operation.alpha.operation_contents_and_result ... ],
--      "signature"?: $Signature }
--    || { "contents": [ $operation.alpha.contents ... ],
--         "signature"?: $Signature }
--
-- This is the return value of the run_operation RPC.
data NoContextOperation = NoContextOperation
  { _noContextOperation_contents :: !(Seq OperationContents)
  , _noContextOperation_signature :: !(Maybe Signature)
  } deriving (Eq, Ord, Show, Typeable)

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
  OpContentsList_Cons :: OpContents ('OpKind_Manager '[k]) -> OpContentsList ('OpKind_Manager (kk ': ks)) -> OpContentsList ('OpKind_Manager (k ': kk ': ks))
  deriving Typeable

deriving instance Eq (OpContentsList a)
deriving instance Ord (OpContentsList a)
deriving instance Show (OpContentsList a)

data OpContents (a :: OpKind) where
  OpContents_Endorsement :: !OpContentsEndorsement -> OpContents 'OpKind_Endorsement
  OpContents_SeedNonceRevelation :: !OpContentsSeedNonceRevelation -> OpContents 'OpKind_SeedNonceRevelation
  OpContents_DoubleEndorsementEvidence :: !OpContentsDoubleEndorsementEvidence -> OpContents 'OpKind_DoubleEndorsementEvidence
  OpContents_DoubleBakingEvidence :: !OpContentsDoubleBakingEvidence -> OpContents 'OpKind_DoubleBakingEvidence
  OpContents_ActivateAccount :: !OpContentsActivateAccount -> OpContents 'OpKind_ActivateAccount
  OpContents_Proposals :: !OpContentsProposals -> OpContents 'OpKind_Proposals
  OpContents_Ballot :: !OpContentsBallot -> OpContents 'OpKind_Ballot
  OpContents_Reveal :: !(OpContentsManager OpContentsReveal) -> OpContents ('OpKind_Manager '[ 'OpKindManager_Reveal])
  OpContents_Transaction :: !(OpContentsManager OpContentsTransaction) -> OpContents ('OpKind_Manager '[ 'OpKindManager_Transaction])
  OpContents_Origination :: !(OpContentsManager OpContentsOrigination) -> OpContents ('OpKind_Manager '[ 'OpKindManager_Origination])
  OpContents_Delegation :: !(OpContentsManager OpContentsDelegation) -> OpContents ('OpKind_Manager '[ 'OpKindManager_Delegation])
  deriving Typeable

deriving instance Eq (OpContents a)
deriving instance Show (OpContents a)

instance Ord (OpContents a) where
  compare = \case
    OpContents_Endorsement op -> \case
      OpContents_Endorsement op2 -> compare op op2
    OpContents_SeedNonceRevelation op -> \case
      OpContents_SeedNonceRevelation op2 -> compare op op2
    OpContents_DoubleEndorsementEvidence op -> \case
      OpContents_DoubleEndorsementEvidence op2 -> compare op op2
    OpContents_DoubleBakingEvidence op -> \case
      OpContents_DoubleBakingEvidence op2 -> compare op op2
    OpContents_ActivateAccount op -> \case
      OpContents_ActivateAccount op2 -> compare op op2
    OpContents_Proposals op -> \case
      OpContents_Proposals op2 -> compare op op2
    OpContents_Ballot op -> \case
      OpContents_Ballot op2 -> compare op op2
    OpContents_Reveal op -> \case
      OpContents_Reveal op2 -> compare op op2
    OpContents_Transaction op -> \case
      OpContents_Transaction op2 -> compare op op2
    OpContents_Origination op -> \case
      OpContents_Origination op2 -> compare op op2
    OpContents_Delegation op -> \case
      OpContents_Delegation op2 -> compare op op2

data OpContentsEndorsement = OpContentsEndorsement
  { _opContentsEndorsement_level :: !RawLevel
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsSeedNonceRevelation = OpContentsSeedNonceRevelation
  { _opContentsSeedNonceRevelation_level :: !RawLevel
  , _opContentsSeedNonceRevelation_nonce :: !(Base16ByteString ByteString)
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsDoubleEndorsementEvidence = OpContentsDoubleEndorsementEvidence
  { _opContentsDoubleEndorsementEvidence_op1 :: !InlinedEndorsement
  , _opContentsDoubleEndorsementEvidence_op2 :: !InlinedEndorsement
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsDoubleBakingEvidence = OpContentsDoubleBakingEvidence
  { _opContentsDoubleBakingEvidence_bh1 :: !BlockHeaderFull
  , _opContentsDoubleBakingEvidence_bh2 :: !BlockHeaderFull
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsActivateAccount = OpContentsActivateAccount
  { _opContentsActivateAccount_pkh :: !Ed25519PublicKeyHash
  , _opContentsActivateAccount_secret :: !(Base16ByteString ByteString)
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsProposals = OpContentsProposals
  { _opContentsProposals_source :: !PublicKeyHash
  , _opContentsProposals_period :: !RawLevel
  , _opContentsProposals_proposals :: !(Seq ProtocolHash)
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsBallot = OpContentsBallot
  { _opContentsBallot_source :: !PublicKeyHash
  , _opContentsBallot_period :: !RawLevel
  , _opContentsBallot_proposal :: !ProtocolHash
  , _opContentsBallot_ballot :: !Ballot
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsManager op = OpContentsManager
  { _opContentsManager_source :: !ContractId
  , _opContentsManager_fee :: !Tez
  , _opContentsManager_counter :: !TezosWord64
  , _opContentsManager_gasLimit :: !TezosWord64
  , _opContentsManager_storageLimit :: !TezosWord64
  , _opContentsManager_operation :: !op
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsReveal = OpContentsReveal
  { _opContentsReveal_publicKey :: !PublicKey
  }
  deriving (Eq, Ord, Show, Typeable)

-- This should only be ascii, so ByteString.C8 actually makes sense
newtype EntrypointName = EntrypointName { unEntrypointName :: C8.ByteString }
  deriving (Eq, Ord, Show, Typeable)

entrypointName :: MonadFail m => C8.ByteString -> m EntrypointName
entrypointName bs =
  if len > 0 && len < 32
    then pure (EntrypointName bs)
    else fail "Entrypoint name not between 1 and 32 characters"
  where
    len = C8.length bs

data Entrypoint
  = EntrypointDefault -- "default"
  | EntrypointRoot -- "root"
  | EntrypointDo -- "do"
  | EntrypointSetDelegate -- "set_delegate"
  | EntrypointRemoveDelegate -- "remove_delegate"
  | EntrypointOther !EntrypointName -- Note this should be < 32 chars TODO: Newtype this to make it safer?
  deriving (Eq, Ord, Show, Typeable)

instance ToJSON EntrypointName where
  toJSON = toJSON . C8.unpack . unEntrypointName

instance FromJSON EntrypointName where
  parseJSON jv = do
    str <- parseJSON @String jv
    when (any (not . isLatin1) str) $ fail "Non latin1 characters found in entrypoint name"
    entrypointName (C8.pack str)

instance ToJSON Entrypoint where
  toJSON ep = toJSON $ case ep of
    EntrypointDefault -> "default"
    EntrypointRoot -> "root"
    EntrypointDo -> "do"
    EntrypointSetDelegate -> "set_delegate"
    EntrypointRemoveDelegate -> "remove_delegate"
    EntrypointOther t -> C8.unpack $ unEntrypointName t

instance FromJSON Entrypoint where
  parseJSON j = parseJSON @String j >>= \case
    "default" -> pure EntrypointDefault
    "root" -> pure EntrypointRoot
    "do" -> pure EntrypointDo
    "set_delegate" -> pure EntrypointSetDelegate
    "remove_delegate" -> pure EntrypointRemoveDelegate
    _ -> EntrypointOther <$> parseJSON j

data OpParameters = OpParameters
  { _opParameters_entrypoint :: !Entrypoint
  , _opParameters_value :: !Expression
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsTransaction = OpContentsTransaction
  { _opContentsTransaction_amount :: !Tez
  , _opContentsTransaction_destination :: !ContractId
  , _opContentsTransaction_parameters :: !(Maybe OpParameters)
  }
  deriving (Eq, Ord, Show, Typeable)


data OpContentsOrigination = OpContentsOrigination
  { _opContentsOrigination_balance :: !Tez
  , _opContentsOrigination_delegate :: !(Maybe PublicKeyHash)
  , _opContentsOrigination_script :: !ContractScript
  }
  deriving (Eq, Ord, Show, Typeable)

data OpContentsDelegation = OpContentsDelegation
  { _opContentsDelegation_delegate :: !PublicKeyHash
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
  , _operationContentsDoubleBakingEvidence_bh1 :: !BlockHeaderFull --  "bh1": { "$ref": "#/definitions/block_header.alpha.full_header" },
  , _operationContentsDoubleBakingEvidence_bh2 :: !BlockHeaderFull --  "bh2": { "$ref": "#/definitions/block_header.alpha.full_header" },
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
          _ -> error ("ToJSON did not produce an object for:" <> show (typeRep (Proxy :: Proxy a)))

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
  , _operationContentsTransaction_parameters :: !(Maybe OpParameters) --  "parameters": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
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
  , _operationContentsOrigination_balance :: !Tez --  "balance": { "$ref": "#/definitions/mutez" },
  , _operationContentsOrigination_delegate :: !(Maybe PublicKeyHash) --  "delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _operationContentsOrigination_script :: !ContractScript --  "script": { "$ref": "#/definitions/scripted.contracts" },
  }
  deriving (Eq, Ord, Show, Typeable)

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
stripEndorsement Operation { _operation_branch = branch, _operation_contents = contents, _operation_signature = sig }
  | length contents /= 1 = Nothing
  | otherwise = case Seq.index contents 0 of
      OperationContents_Endorsement OperationContentsEndorsement { _operationContentsEndorsement_level = level } ->
        Just $ Op { _op_branch = branch, _op_contents = OpContentsList_Single $ OpContents_Endorsement $ OpContentsEndorsement level, _op_signature = sig }
      _ -> Nothing

outlineEndorsement :: InlinedEndorsement -> Op 'OpKind_Endorsement
outlineEndorsement InlinedEndorsement { _inlinedEndorsement_branch = branch, _inlinedEndorsement_operations = contents, _inlinedEndorsement_signature = sig }
  = case contents of
      InlinedEndorsementContents { _inlinedEndorsementContents_level = level } ->
        Op { _op_branch = branch, _op_contents = OpContentsList_Single $ OpContents_Endorsement $ OpContentsEndorsement level, _op_signature = sig }

instance B.TezosBinary (Some OpKindTag) where
  put t = withSome t $ \case
    OpKindTag_Endorsement -> B.put @Word8 0
    OpKindTag_SeedNonceRevelation -> B.put @Word8 1
    OpKindTag_DoubleEndorsementEvidence -> B.put @Word8 2
    OpKindTag_DoubleBakingEvidence -> B.put @Word8 3
    OpKindTag_ActivateAccount -> B.put @Word8 4
    OpKindTag_Proposals -> B.put @Word8 5
    OpKindTag_Ballot -> B.put @Word8 6
    OpKindTag_Manager OpKindManagerTag_Reveal -> B.put @Word8 107
    OpKindTag_Manager OpKindManagerTag_Transaction -> B.put @Word8 108
    OpKindTag_Manager OpKindManagerTag_Origination -> B.put @Word8 109
    OpKindTag_Manager OpKindManagerTag_Delegation -> B.put @Word8 110
  get = B.get @Word8 >>= \case
    0 -> pure $ Some OpKindTag_Endorsement
    1 -> pure $ Some OpKindTag_SeedNonceRevelation
    2 -> pure $ Some OpKindTag_DoubleEndorsementEvidence
    3 -> pure $ Some OpKindTag_DoubleBakingEvidence
    4 -> pure $ Some OpKindTag_ActivateAccount
    5 -> pure $ Some OpKindTag_Proposals
    6 -> pure $ Some OpKindTag_Ballot
    107 -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Reveal)
    108 -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Transaction)
    109 -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Origination)
    110 -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Delegation)
    x -> fail $ "unknown operation tag: " <> show x

instance B.TezosBinary OpContentsEndorsement where
  put = B.puts _opContentsEndorsement_level
  get = OpContentsEndorsement <$> B.get

nonce_size :: Int
nonce_size = 32

instance B.TezosBinary OpContentsSeedNonceRevelation where
  put = B.puts _opContentsSeedNonceRevelation_level
    <** B.puts _opContentsSeedNonceRevelation_nonce
  get = OpContentsSeedNonceRevelation <$> B.get <*> isolate nonce_size B.get

instance B.TezosBinary InlinedEndorsementContents where
  put = B.puts _inlinedEndorsementContents_level
  get = InlinedEndorsementContents <$> B.get

instance B.TezosBinary InlinedEndorsement where
  put = B.puts _inlinedEndorsement_branch
    <** B.puts _inlinedEndorsement_operations
    <** B.puts _inlinedEndorsement_signature
  get = InlinedEndorsement <$> B.get <*> B.get <*> B.get

instance B.TezosBinary OpContentsDoubleEndorsementEvidence where
  put = B.puts (B.DynamicSize . _opContentsDoubleEndorsementEvidence_op1)
    <** B.puts (B.DynamicSize . _opContentsDoubleEndorsementEvidence_op2)
  get = OpContentsDoubleEndorsementEvidence <$> fmap B.unDynamicSize B.get <*> fmap B.unDynamicSize B.get

instance B.TezosBinary OpContentsDoubleBakingEvidence where
  put = B.puts (B.DynamicSize . _opContentsDoubleBakingEvidence_bh1)
    <** B.puts (B.DynamicSize . _opContentsDoubleBakingEvidence_bh2)
  get = OpContentsDoubleBakingEvidence <$> fmap B.unDynamicSize B.get <*> fmap B.unDynamicSize B.get

activation_code_size :: Int
activation_code_size = hashSize (Proxy @'HashType_Ed25519PublicKeyHash)

instance B.TezosBinary OpContentsActivateAccount where
  put = B.puts _opContentsActivateAccount_pkh
    <** B.puts _opContentsActivateAccount_secret
  get = OpContentsActivateAccount <$> B.get <*> isolate activation_code_size B.get

instance B.TezosBinary OpContentsProposals where
  put = B.puts _opContentsProposals_source
    <** B.puts _opContentsProposals_period
    <** B.puts (B.DynamicSize . _opContentsProposals_proposals)
  get = OpContentsProposals <$> B.get <*> B.get <*> fmap B.unDynamicSize B.get

instance B.TezosBinary Ballot where
  put = \case
    Ballot_Yay -> B.put @Word8 0
    Ballot_Nay -> B.put @Word8 1
    Ballot_Pass -> B.put @Word8 2
  get = B.get @Word8 >>= \case
    0 -> pure Ballot_Yay
    1 -> pure Ballot_Nay
    2 -> pure Ballot_Pass
    x -> fail $ "unknown ballot tag: " <> show x

instance B.TezosBinary OpContentsBallot where
  put = B.puts _opContentsBallot_source
    <** B.puts _opContentsBallot_period
    <** B.puts _opContentsBallot_proposal
    <** B.puts _opContentsBallot_ballot
  get = OpContentsBallot <$> B.get <*> B.get <*> B.get <*> B.get

instance B.TezosBinary OpContentsReveal where
  put = B.puts _opContentsReveal_publicKey
  get = OpContentsReveal <$> B.get

instance B.TezosBinary EntrypointName where
  put (EntrypointName bs) = B.put @Word8 (fromIntegral $ BS.length bs) *> traverse_ B.put (BS.unpack bs)
  get = do
    lenWord <- B.get @Word8
    let lenInt = fromIntegral lenWord
    when (lenInt < 0 || lenInt > 31) $ fail ("Entry point len not between 1 and 31: " <> show lenInt)
    wordsList <- replicateM lenInt (B.get @Word8)
    entrypointName (BS.pack wordsList)

instance B.TezosBinary Entrypoint where
  put = \case
    EntrypointDefault -> B.put @Word8 0
    EntrypointRoot -> B.put @Word8 1
    EntrypointDo -> B.put @Word8 2
    EntrypointSetDelegate -> B.put @Word8 3
    EntrypointRemoveDelegate -> B.put @Word8 4
    EntrypointOther epn -> B.put @Word8 255 *> B.put epn
    
  get = do
    epWord <- B.get @Word8
    case epWord of
      0 -> pure EntrypointDefault
      1 -> pure EntrypointRoot
      2 -> pure EntrypointDo
      3 -> pure EntrypointSetDelegate
      4 -> pure EntrypointRemoveDelegate
      255 -> EntrypointOther <$> B.get
      _ -> fail $ "Unreserved entrypoint code: " <> show epWord

instance B.TezosBinary OpParameters where
  put = B.puts _opParameters_entrypoint
    <** B.puts _opParameters_value
  get = OpParameters
    <$> B.get
    <*> B.get

instance B.TezosBinary OpContentsTransaction where
  put = B.puts _opContentsTransaction_amount
    <** B.puts _opContentsTransaction_destination
    <** B.puts (fmap B.DynamicSize . _opContentsTransaction_parameters)
  get = pure OpContentsTransaction
    <*> B.get
    <*> B.get
    <*> fmap (fmap B.unDynamicSize) B.get

instance B.TezosBinary OpContentsOrigination where
  put = B.puts _opContentsOrigination_balance
    <** B.puts _opContentsOrigination_delegate
    <** B.puts _opContentsOrigination_script
  get = pure OpContentsOrigination
    <*> B.get
    <*> B.get
    <*> B.get

instance B.TezosBinary OpContentsDelegation where
  put = B.puts _opContentsDelegation_delegate
  get = OpContentsDelegation <$> B.get

instance B.TezosBinary op => B.TezosBinary (OpContentsManager op) where
  put = B.puts _opContentsManager_source
    <** B.puts _opContentsManager_fee
    <** B.puts _opContentsManager_counter
    <** B.puts _opContentsManager_gasLimit
    <** B.puts _opContentsManager_storageLimit
    <** B.puts _opContentsManager_operation
  get = pure OpContentsManager
    <*> B.get
    <*> B.get
    <*> B.get
    <*> B.get
    <*> B.get
    <*> B.get

instance FromJSON op => FromJSON (OpContentsManager op) where
  parseJSON o = withObject "OpContentsManager" `flip` o $ \v -> OpContentsManager
    <$> v .: "source"
    <*> v .: "fee"
    <*> v .: "counter"
    <*> v .: "gas_limit"
    <*> v .: "storage_limit"
    <*> parseJSON o

-- FIXME Aeson should have a ToJSONObject class for things that are objects!!
jsonAddKeys :: [(Text,Value)] -> Value -> Value
jsonAddKeys keys = \case
  Object o -> Object $ HashMap.fromList keys <> o
  _ -> String "YOU TRIED TO USE jsonAddKeys ON SOMETHING OTHER THAN AN OBJECT"

instance ToJSON op => ToJSON (OpContentsManager op) where
  toJSON v = jsonAddKeys
    [ "source" .= _opContentsManager_source v
    , "fee" .= _opContentsManager_fee v
    , "counter" .= _opContentsManager_counter v
    , "gas_limit" .= _opContentsManager_gasLimit v
    , "storage_limit" .= _opContentsManager_storageLimit v
    ]
    $ toJSON $ _opContentsManager_operation v

instance B.TezosBinary (OpContents 'OpKind_Endorsement) where
  put = \case
    OpContents_Endorsement op -> B.put @Word8 0 <* B.put op
  get = B.get @Word8 >>= \case
    0 -> OpContents_Endorsement <$> B.get
    _ -> fail "not an endorsement"

instance B.TezosBinary (DSum OpKindTag OpContents) where
  put (t :=> op) = B.put (Some t) *>
    case op of
      OpContents_Endorsement opc -> B.put opc
      OpContents_SeedNonceRevelation opc -> B.put opc
      OpContents_DoubleEndorsementEvidence opc -> B.put opc
      OpContents_DoubleBakingEvidence opc -> B.put opc
      OpContents_ActivateAccount opc -> B.put opc
      OpContents_Proposals opc -> B.put opc
      OpContents_Ballot opc -> B.put opc
      OpContents_Reveal opc -> B.put opc
      OpContents_Transaction opc -> B.put opc
      OpContents_Origination opc -> B.put opc
      OpContents_Delegation opc -> B.put opc
  get = B.get >>= \case
    Some t -> (t :=>) <$> case t of
      OpKindTag_Endorsement -> OpContents_Endorsement <$> B.get
      OpKindTag_SeedNonceRevelation -> OpContents_SeedNonceRevelation <$> B.get
      OpKindTag_DoubleEndorsementEvidence -> OpContents_DoubleEndorsementEvidence <$> B.get
      OpKindTag_DoubleBakingEvidence -> OpContents_DoubleBakingEvidence <$> B.get
      OpKindTag_ActivateAccount -> OpContents_ActivateAccount <$> B.get
      OpKindTag_Proposals -> OpContents_Proposals <$> B.get
      OpKindTag_Ballot -> OpContents_Ballot <$> B.get
      OpKindTag_Manager OpKindManagerTag_Reveal -> OpContents_Reveal <$> B.get
      OpKindTag_Manager OpKindManagerTag_Transaction -> OpContents_Transaction <$> B.get
      OpKindTag_Manager OpKindManagerTag_Origination -> OpContents_Origination <$> B.get
      OpKindTag_Manager OpKindManagerTag_Delegation -> OpContents_Delegation <$> B.get

instance ToJSON (Some OpKindTag) where
  toJSON = \case
    Some OpKindTag_Endorsement -> String "endorsement"
    Some OpKindTag_SeedNonceRevelation -> String "seed_nonce_revelation"
    Some OpKindTag_DoubleEndorsementEvidence -> String "double_endorsement_evidence"
    Some OpKindTag_DoubleBakingEvidence -> String "double_baking_evidence"
    Some OpKindTag_ActivateAccount -> String "activate_account"
    Some OpKindTag_Proposals -> String "proposals"
    Some OpKindTag_Ballot -> String "ballot"
    Some (OpKindTag_Manager OpKindManagerTag_Reveal) -> String "reveal"
    Some (OpKindTag_Manager OpKindManagerTag_Transaction) -> String "transaction"
    Some (OpKindTag_Manager OpKindManagerTag_Origination) -> String "origination"
    Some (OpKindTag_Manager OpKindManagerTag_Delegation) -> String "delegation"

instance FromJSON (Some OpKindTag) where
  parseJSON = \case
    String "endorsement" -> pure $ Some OpKindTag_Endorsement
    String "seed_nonce_revelation" -> pure $ Some OpKindTag_SeedNonceRevelation
    String "reveal" -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Reveal)
    String "transaction" -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Transaction)
    String "origination" -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Origination)
    String "delegation" -> pure $ Some (OpKindTag_Manager OpKindManagerTag_Delegation)
    _ -> fail "not a supported operation kind"

instance ToJSON (DSum OpKindTag OpContents) where
  toJSON (t :=> op) = jsonAddKeys [ "kind" .= Some t ] $ case op of
    OpContents_Endorsement c -> toJSON c
    OpContents_SeedNonceRevelation c -> toJSON c
    OpContents_DoubleEndorsementEvidence c -> toJSON c
    OpContents_DoubleBakingEvidence c -> toJSON c
    OpContents_ActivateAccount c -> toJSON c
    OpContents_Proposals c -> toJSON c
    OpContents_Ballot c -> toJSON c
    OpContents_Reveal c -> toJSON c
    OpContents_Transaction c -> toJSON c
    OpContents_Origination c -> toJSON c
    OpContents_Delegation c -> toJSON c

instance FromJSON (DSum OpKindTag OpContents) where
  parseJSON v = do
    tt <- withObject "operation contents" (.: "kind") v
    case tt of
      Some t@OpKindTag_Endorsement -> (t :=>) . OpContents_Endorsement <$> parseJSON v
      Some t@OpKindTag_SeedNonceRevelation -> (t :=>) . OpContents_SeedNonceRevelation <$> parseJSON v
      Some t@OpKindTag_DoubleEndorsementEvidence -> (t :=>) . OpContents_DoubleEndorsementEvidence <$> parseJSON v
      Some t@OpKindTag_DoubleBakingEvidence -> (t :=>) . OpContents_DoubleBakingEvidence <$> parseJSON v
      Some t@OpKindTag_ActivateAccount -> (t :=>) . OpContents_ActivateAccount <$> parseJSON v
      Some t@OpKindTag_Proposals -> (t :=>) . OpContents_Proposals <$> parseJSON v
      Some t@OpKindTag_Ballot -> (t :=>) . OpContents_Ballot <$> parseJSON v
      Some t@(OpKindTag_Manager OpKindManagerTag_Reveal) -> (t :=>) . OpContents_Reveal <$> parseJSON v
      Some t@(OpKindTag_Manager OpKindManagerTag_Transaction) -> (t :=>) . OpContents_Transaction <$> parseJSON v
      Some t@(OpKindTag_Manager OpKindManagerTag_Origination) -> (t :=>) . OpContents_Origination <$> parseJSON v
      Some t@(OpKindTag_Manager OpKindManagerTag_Delegation) -> (t :=>) . OpContents_Delegation <$> parseJSON v

instance B.TezosBinary (OpContentsList 'OpKind_Endorsement) where
  put = \case
    OpContentsList_Single op -> B.put op
  get = OpContentsList_Single <$> B.get

instance ToJSON (DSum OpsKindTag OpContentsList) where
  toJSON = Array . Vector.fromList . toJSONs
    where
    toJSONs = \case
      otl :=> ocl -> case otl of
        OpsKindTag_Cons (t :: OpKindTag ('OpKind_Manager '[k])) (ts :: OpsKindTag ('OpKind_Manager (kk ': ks))) ->
          case (ocl :: OpContentsList ('OpKind_Manager (k ': kk ': ks))) of
            OpContentsList_Cons op ops ->
              toJSON (t :=> op) : toJSONs (ts :=> ops)
            OpContentsList_Single op -> case op of {}
        OpsKindTag_Single (t :: OpKindTag k) ->
          case (ocl :: OpContentsList k) of
            OpContentsList_Single op ->
              [toJSON $ t :=> op]
            OpContentsList_Cons (_ :: OpContents ('OpKind_Manager '[kay])) (_ :: OpContentsList ('OpKind_Manager (kk ': ks))) ->
              case (t :: OpKindTag ('OpKind_Manager (kay ': kk ': ks))) of {}

instance FromJSON (DSum OpsKindTag OpContentsList) where
  parseJSON = withArray "contents list" $ (checkBatch . toList) <=< traverse parseJSON

checkBatch :: MonadFail m => [DSum OpKindTag OpContents] -> m (DSum OpsKindTag OpContentsList)
checkBatch = \case
  [t :=> op] -> pure $ OpsKindTag_Single t :=> OpContentsList_Single op
  [] -> fail "empty batch not supported"
  v:vs -> case v of
    t@(OpKindTag_Manager _) :=> op -> checkBatch vs >>= \case
      ts@(OpsKindTag_Single (OpKindTag_Manager _)) :=> ops ->
        return $ OpsKindTag_Cons t ts :=> OpContentsList_Cons op ops
      OpsKindTag_Single _ :=> _ -> fail "only manager ops allowed in batches"
      ts@(OpsKindTag_Cons _ _) :=> ops ->
        return $ OpsKindTag_Cons t ts :=> OpContentsList_Cons op ops
    _ :=> _ -> fail "only manager ops allowed in batches"

instance B.TezosBinary (DSum OpsKindTag OpContentsList) where
  put = \case
    otl :=> ocl -> case otl of
      OpsKindTag_Cons (t :: OpKindTag ('OpKind_Manager '[k])) (ts :: OpsKindTag ('OpKind_Manager (kk ': ks))) ->
        case (ocl :: OpContentsList ('OpKind_Manager (k ': kk ': ks))) of
          OpContentsList_Cons op ops ->
            B.put (t :=> op) <* B.put (ts :=> ops)
          OpContentsList_Single op -> case op of {}
      OpsKindTag_Single (t :: OpKindTag k) ->
        case (ocl :: OpContentsList k) of
          OpContentsList_Single op ->
            B.put $ t :=> op
          OpContentsList_Cons (_ :: OpContents ('OpKind_Manager '[kay])) (_ :: OpContentsList ('OpKind_Manager (kk ': ks))) ->
            case (t :: OpKindTag ('OpKind_Manager (kay ': kk ': ks))) of {}
  get = B.get >>= (checkBatch . toList @Seq)

instance B.TezosUnsignedBinary (Op 'OpKind_Endorsement) where
  putUnsigned = shellHeaderEncoding <** B.puts _op_contents
    where
      shellHeaderEncoding = B.puts _op_branch
  getUnsigned = shellHeaderDecoding <*> B.get <*> pure Nothing
    where
      shellHeaderDecoding = Op <$> B.get

instance ToJSON (DSum OpsKindTag Op) where
  toJSON = \case
    tag :=> op -> object $
      [ "branch" .= _op_branch op
      , "contents" .= (tag :=> _op_contents op)
      ] <> toList (("signature" .=) <$> _op_signature op)

instance FromJSON (DSum OpsKindTag Op) where
  parseJSON = withObject "operation" $ \o -> do
    branch <- o .: "branch"
    taggedContents <- o .: "contents"
    signature <- o .:? "signature"
    case taggedContents of
      t :=> ops -> pure $ t :=> Op { _op_branch = branch, _op_contents = ops , _op_signature = signature }

instance ToJSON (DSum OpsKindTag ProtoOp) where
  toJSON = \case
    tag :=> prop -> object $
      [ "protocol" .= _protoOp_protocol prop
      , "branch" .= _protoOp_branch prop
      , "contents" .= (tag :=> _protoOp_contents prop)
      ] <> toList (("signature" .=) <$> _protoOp_signature prop)

instance FromJSON (DSum OpsKindTag ProtoOp) where
  parseJSON = withObject "operation" $ \o -> do
    protocol <- o .: "protocol"
    branch <- o .: "branch"
    taggedContents <- o .: "contents"
    signature <- o .:? "signature"
    case taggedContents of
      t :=> ops -> pure $ t :=> ProtoOp { _protoOp_protocol = protocol, _protoOp_branch = branch, _protoOp_contents = ops , _protoOp_signature = signature }

instance ToJSON (DSum OpsKindTag PendingOp) where
  toJSON = \case
    tag :=> prop -> object
      [ "hash" .= _pendingOp_hash prop
      , "branch" .= _pendingOp_branch prop
      , "contents" .= (tag :=> _pendingOp_contents prop)
      , "signature" .= _pendingOp_signature prop
      ]

instance FromJSON (DSum OpsKindTag PendingOp) where
  parseJSON = withObject "operation" $ \o -> do
    hash <- o .: "hash"
    branch <- o .: "branch"
    taggedContents <- o .: "contents"
    signature <- o .: "signature"
    case taggedContents of
      t :=> ops -> pure $ t :=> PendingOp { _pendingOp_hash = hash, _pendingOp_branch = branch, _pendingOp_contents = ops , _pendingOp_signature = signature }

instance ToJSON (DSum OpsKindTag ErroredOp) where
  toJSON = \case
    tag :=> op -> object
      [ "protocol" .= _erroredOp_protocol op
      , "branch" .= _erroredOp_branch op
      , "contents" .= (tag :=> _erroredOp_contents op)
      , "signature" .= _erroredOp_signature op
      , "error" .= _erroredOp_error op
      ]

instance FromJSON (DSum OpsKindTag ErroredOp) where
  parseJSON = withObject "operation" $ \o -> do
    protocol <- o .: "protocol"
    branch <- o .: "branch"
    taggedContents <- o .: "contents"
    signature <- o .: "signature"
    errorV <- o .: "error"
    case taggedContents of
      t :=> ops -> pure $ t :=> ErroredOp { _erroredOp_protocol = protocol, _erroredOp_branch = branch, _erroredOp_contents = ops , _erroredOp_signature = signature, _erroredOp_error = errorV }

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
      B.putUnsigned op *> traverse_ B.put (_op_signature body)
  -- XXX get doesn't really work because it will try to parse the signature as a batched operation
  get = B.getUnsigned >>= \case
    (t :=> op) -> do
      sig <- B.get
      pure $ t :=> op { _op_signature = Just sig }

concat <$> traverse deriveTezosJson
  [ ''Operation
  , ''NoContextOperation
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
  , ''OperationContentsOrigination
  , ''OpContentsOrigination
  , ''OperationContentsDelegation, ''OperationResultDelegation
  , ''OpContentsEndorsement
  , ''OpContentsSeedNonceRevelation
  , ''OpContentsDoubleEndorsementEvidence
  , ''OpContentsDoubleBakingEvidence
  , ''OpContentsActivateAccount
  , ''OpContentsProposals
  , ''OpContentsBallot
  , ''OpContentsReveal
  , ''OpContentsTransaction
  , ''OpContentsDelegation
  , ''OpParameters
  ]


instance (ToJSON a, Typeable a) => ToJSON (ManagerOperationMetadata a) where
  toJSON = $(Aeson.mkToJSON tezosJsonOptions ''ManagerOperationMetadata)
  toEncoding = $(Aeson.mkToEncoding tezosJsonOptions ''ManagerOperationMetadata)

instance (FromJSON a, Typeable a) => FromJSON (ManagerOperationMetadata a) where
  parseJSON = $(Aeson.mkParseJSON tezosJsonOptions ''ManagerOperationMetadata)

fmap concat $ sequence
  [ concat <$> traverse (Aeson.deriveToJSON tezosJsonOptions)
    [ ''OperationResultOrigination
    , ''OperationResultTransaction
    ]
  , concat <$> traverse makePrisms
    [ ''OperationContents
    , ''Entrypoint
    ]
  , concat <$> traverse makeLenses
    [ 'Operation
    , 'NoContextOperation
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
    , 'OpContentsEndorsement
    , 'OpContentsTransaction
    , 'OpContentsOrigination
    , 'SeedNonceRevelationMetadata
    , 'OpParameters
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

mkOp :: BlockHash -> DSum OpsKindTag OpContentsList -> Maybe Signature -> DSum OpsKindTag Op
mkOp branch (ts :=> ops) sig = ts :=> Op branch ops sig

mkSignedOp :: BlockHash -> DSum OpsKindTag OpContentsList -> Signature -> DSum OpsKindTag Op
mkSignedOp branch contents = mkOp branch contents . Just

mkUnsignedOp :: BlockHash -> DSum OpsKindTag OpContentsList -> DSum OpsKindTag Op
mkUnsignedOp branch contents = mkOp branch contents Nothing

batchOperations :: NonEmpty (DSum BatchableOp OpContents) -> DSum OpsKindTag OpContentsList
batchOperations (x :| xs) = go x xs & \(Compose t :=> Compose l) -> t :=> l
  where
  go :: DSum BatchableOp OpContents -> [DSum BatchableOp OpContents] -> DSum (Compose OpsKindTag 'OpKind_Manager) (Compose OpContentsList 'OpKind_Manager)
  go (BatchableOp t :=> op) = \case
    [] -> Compose (OpsKindTag_Single (OpKindTag_Manager t)) :=> Compose (OpContentsList_Single op)
    y:ys -> go y ys & \case
      Compose ts :=> Compose ops -> Compose (consOpTags t ts) :=> Compose (consOpContents op ops)

consOpTags :: OpKindManagerTag k -> OpsKindTag ('OpKind_Manager ks) -> OpsKindTag ('OpKind_Manager (k ': ks))
consOpTags x = \case
  xs@(OpsKindTag_Single (OpKindTag_Manager _)) -> OpsKindTag_Cons (OpKindTag_Manager x) xs
  xs@(OpsKindTag_Cons _ _) -> OpsKindTag_Cons (OpKindTag_Manager x) xs

consOpContents :: OpContents ('OpKind_Manager '[k]) -> OpContentsList ('OpKind_Manager ks) -> OpContentsList ('OpKind_Manager (k ': ks))
consOpContents x = \case
  xs@(OpContentsList_Single (OpContents_Reveal _)) -> OpContentsList_Cons x xs
  xs@(OpContentsList_Single (OpContents_Transaction _)) -> OpContentsList_Cons x xs
  xs@(OpContentsList_Single (OpContents_Origination _)) -> OpContentsList_Cons x xs
  xs@(OpContentsList_Single (OpContents_Delegation _)) -> OpContentsList_Cons x xs
  xs@(OpContentsList_Cons _ _) -> OpContentsList_Cons x xs

batchUnfoldrM :: forall m a. Monad m => (a -> m (DSum BatchableOp OpContents, Maybe a)) -> a -> m (DSum OpsKindTag OpContentsList)
batchUnfoldrM f x = go x <&> \(Compose ts :=> Compose ops) -> ts :=> ops
  where
  go :: a -> m (DSum (Compose OpsKindTag 'OpKind_Manager) (Compose OpContentsList 'OpKind_Manager))
  go seed = f seed >>= \case
    (BatchableOp t :=> op, next) -> traverse go next <&> \case
      Nothing -> Compose (OpsKindTag_Single (OpKindTag_Manager t)) :=> Compose (OpContentsList_Single op)
      Just (Compose ts :=> Compose ops) -> Compose (consOpTags t ts) :=> Compose (consOpContents op ops)
