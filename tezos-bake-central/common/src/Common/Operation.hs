{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Operation where


import Control.Monad
import Data.Attoparsec.ByteString
import Data.Int
import Data.Semigroup
import Data.Typeable
import GHC.Generics
import GHC.Word
import qualified Data.ByteString as BS

import Common.BlockHeader
import Common.PublicKey
import Common.PublicKeyHash
import Common.Script
import Common.Signature
import Common.TaggedHash
import Common.Tez
import Common.TezosBinary
import Common.Vote

newtype ShellHeader = ShellHeader { _shellHeader_branch :: BlockHash }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary ShellHeader where
  parseBinary = ShellHeader <$> parseBinary <?> "branch"
  encodeBinary (ShellHeader x) = encodeBinary x


type Counter = Int32
type RawLevel = Int32 -- ^ level: Raw_level_repr.t ;
type Seed = BS.ByteString -- ^     nonce: Seed_repr.nonce ;
type BlindedPublicKeyHash = BS.ByteString -- ^ Blinded_public_key_hash.secret


data Operation = Operation
  { shell :: ShellHeader
  , contents :: ProtoOperation
  , signature :: Maybe Signature -- ^  signature: Signature.t option ;
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary Operation where
  parseBinary = Operation
    <$> (parseBinary <?> "shell")
    <*> (parseBinary <?> "contents")
    *> (parseBinary <?> "signature")

  encodeBinary (Operation sh c sig)
    = encodeBinary sh
    <> encodeBinary c
    <> encodeBinary sig

data ProtoOperation
  = SourcedOperations SourcedOperations
  | AnonymousOperations [AnonymousOperation]
  deriving (Eq, Ord, Show, Generic, Typeable)


instance TezosBinary ProtoOperation where
  parseBinary = parseTagged 0 "signed_operation" SourcedOperations
        `mplus` parseTagged 1 "unsigned_operation" AnonymousOperations

  encodeBinary (SourcedOperations x) = encodeBinary (0 :: Word8) <> encodeBinary x
  encodeBinary (AnonymousOperations x) = encodeBinary (1 :: Word8) <> encodeBinary x

data AnonymousOperation
  = SeedNonceRevelation 
    { _seedNonceRevelation_level :: RawLevel
    , _seedNonceRevelation_nonce :: Seed -- ^     nonce: Seed_repr.nonce ;
    }
  | DoubleEndorsementEvidence
    { _doubleEndorsementEvidence_op1 :: Operation
    , _doubleEndorsementEvidence_op2 :: Operation
    }
  | DoubleBakingEvidence
    { _doubleBakingEvidence_bh1 :: BlockHeader
    , _doubleBakingEvidence_bh2 :: BlockHeader
    }
  | Activation
    { _activation_id :: Ed25519PublicKeyHash
    , _activation_secret :: BlindedPublicKeyHash -- ^ secret: Blinded_public_key_hash.secret ;
    }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary AnonymousOperation where
  parseBinary = parseTagged2 0 "SeedNonceRevelation" SeedNonceRevelation
        `mplus` parseTagged2 1 "DoubleEndorsementEvidence" DoubleEndorsementEvidence
        `mplus` parseTagged2 2 "DoubleBakingEvidence" DoubleBakingEvidence
        `mplus` parseTagged2 3 "Activation" Activation

  encodeBinary (SeedNonceRevelation x y) = encodeBinary
    (0 :: Word8) <> encodeBinary x <> encodeBinary y
  encodeBinary (DoubleEndorsementEvidence x y) = encodeBinary
    (1 :: Word8) <> encodeBinary x <> encodeBinary y
  encodeBinary (DoubleBakingEvidence x y) = encodeBinary
    (2 :: Word8) <> encodeBinary x <> encodeBinary y
  encodeBinary (Activation x y) = encodeBinary
    (3 :: Word8) <> encodeBinary x <> encodeBinary y


data Contract
  = Implicit PublicKeyHash
  | Originated ContractHash -- ^ Contract_hash.t
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary Contract where
  parseBinary = parseTagged 0 "Implicit" Implicit
        `mplus` parseTagged 1 "Originated" Originated

  encodeBinary (Implicit x) = encodeBinary
    (0 :: Word8) <> encodeBinary x
  encodeBinary (Originated x) = encodeBinary
    (1 :: Word8) <> encodeBinary x

data SourcedOperations
  = ConsensusOperation ConsensusOperation
  | AmendmentOperation
    { _amendmentOperation_source :: PublicKeyHash -- ^ source: Signature.Public_key_hash.t ;
    , _amendmentOperation_operation :: AmendmentOperation
    }
  | ManagerOperations
    { _managerOperations_contract :: Contract
    , _managerOperations_fee :: Tezzies
    , _managerOperations_counter :: Counter
    , _managerOperations_operations :: [ManagerOperation]
    }
  | DictatorOperation DictatorOperation
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary SourcedOperations where
  parseBinary = parseTagged  0 "ConsensusOperation" ConsensusOperation
        `mplus` parseTagged2 1 "AmendmentOperation" AmendmentOperation
        `mplus` parseTagged4 2 "ManagerOperations" ManagerOperations
        `mplus` parseTagged  3 "DictatorOperation" DictatorOperation

  encodeBinary = \case
    ConsensusOperation x -> encodeBinary
      (0 :: Word8) <> encodeBinary x
    AmendmentOperation x y -> encodeBinary
      (1 :: Word8) <> encodeBinary x <> encodeBinary y
    ManagerOperations x y z w -> encodeBinary
      (2 :: Word8) <> encodeBinary x <> encodeBinary y <> encodeBinary z <> encodeBinary w
    DictatorOperation x -> encodeBinary
      (3 :: Word8) <> encodeBinary x

data ConsensusOperation
  = Endorsements
    { _endorsements_block :: BlockHash
    , _endorsements_level :: RawLevel
    , _endorsements_slots :: [Int32]
    }
  deriving (Eq, Ord, Show, Generic, Typeable)
instance TezosBinary ConsensusOperation where
  parseBinary = Endorsements
    <$> (parseBinary <?> "block")
    <*> (parseBinary <?> "level")
    <*> (parseBinary <?> "slots")

  encodeBinary (Endorsements block level slots) =
    encodeBinary block <> encodeBinary level <> encodeBinary slots


data AmendmentOperation
  = Proposals
    { _proposals_period :: VotingPeriod
    , _proposals_proposals :: [ProtocolHash]
    }
  | Ballot
    { _ballot_period :: VotingPeriod
    , _ballot_proposal :: ProtocolHash
    , _ballot_ballot :: Ballot
    }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary AmendmentOperation where
  parseBinary = parseTagged2 0 "Proposals" Proposals
        `mplus` parseTagged3 1 "Ballot" Ballot
  encodeBinary (Proposals period proposals) = encodeBinary 
    (0 :: Word8) <> encodeBinary period <> encodeBinary proposals
  encodeBinary (Ballot period proposal ballot ) = encodeBinary
    (1 :: Word8) <> encodeBinary period <> encodeBinary proposal <> encodeBinary ballot


data ManagerOperation
  = Reveal PublicKey -- ^ Signature.Public_key.t
  | Transaction 
    { _transaction_amount :: Tezzies
    , _transaction_parameters :: Maybe Script
    , _transaction_destination :: Contract
    }
  | Origination
    { _origination_manager :: PublicKeyHash -- ^ manager: Signature.Public_key_hash.t ;
    , _origination_delegate :: PublicKeyHash
    , _origination_script :: Maybe Script
    , _origination_spendable :: Bool
    , _origination_delgatable :: Bool
    , _origination_credit :: Tezzies
    }
  | Delegation (Maybe PublicKeyHash)
  deriving (Eq, Ord, Show, Generic, Typeable)
instance TezosBinary ManagerOperation where
  parseBinary = parseTagged 0 "Reveal" Reveal
        `mplus` parseTagged3 1 "Transaction" Transaction
        `mplus` parseTagged6 2 "Origination" Origination
        `mplus` parseTagged 3 "Delegation" Delegation

  encodeBinary = \case
    Reveal pk -> encodeBinary
      (0 :: Word8)
        <> encodeBinary pk
    Transaction amount parameters destination -> encodeBinary
      (1 :: Word8)
        <> encodeBinary amount
        <> encodeBinary parameters
        <> encodeBinary destination
    Origination manager delegate script spendable delgatable credit -> encodeBinary
      (2 :: Word8)
        <> encodeBinary manager
        <> encodeBinary delegate
        <> encodeBinary script
        <> encodeBinary spendable
        <> encodeBinary delgatable
        <> encodeBinary credit
    Delegation pkh -> encodeBinary
      (3 :: Word8) <> encodeBinary pkh

data DictatorOperation
  = Activate ProtocolHash
  | ActivateTestChain ProtocolHash
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary DictatorOperation where
  parseBinary = parseTagged 0 "Activate" Activate
        `mplus` parseTagged 1 "ActivateTestChain" ActivateTestChain

  encodeBinary = \case
    Activate x -> encodeBinary (0 :: Word8) <> encodeBinary x
    ActivateTestChain x -> encodeBinary (1 :: Word8) <> encodeBinary x


acceptablePasses :: ProtoOperation -> [Int]
acceptablePasses = \case
  SourcedOperations (ConsensusOperation _) -> [0]
  SourcedOperations (AmendmentOperation _ _) -> [1]
  SourcedOperations (DictatorOperation _) -> [1]
  AnonymousOperations _ -> [2]
  SourcedOperations (ManagerOperations _ _ _ _) -> [3]

sumFees :: ProtoOperation -> Tezzies
sumFees = \case
  SourcedOperations mOp@(ManagerOperations _ _ _ _) -> _managerOperations_fee mOp
  _ -> 0
