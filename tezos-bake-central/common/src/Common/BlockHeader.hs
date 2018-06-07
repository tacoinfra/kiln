{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Common.BlockHeader where

import Data.Attoparsec.ByteString
import Data.Int
import Data.Semigroup
import Data.Time
import Data.Typeable
import GHC.Generics
import GHC.Word

import Common.Fitness
import Common.TaggedHash
import Common.TezosBinary

-- TODO: split this into ShellHeader/AlphaProtoHeader/etc
data BlockHeader = BlockHeader
  { _blockHeader_level :: Int32
  , _blockHeader_proto :: Word8
  , _blockHeader_predecessor :: BlockHash
  , _blockHeader_timestamp :: UTCTime
  , _blockHeader_validationPass :: Word8
  , _blockHeader_operationsHash :: OperationListListHash
  , _blockHeader_fitness :: Fitness
  , _blockHeader_context :: ContextHash
  , _blockHeader_priority :: Word16
  , _blockHeader_proofOfWorkNonce :: Word64
  , _blockHeader_seedNonceHash :: Maybe NonceHash
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- TODO:  generate this automatically.;  see tezos/binary-description branch...
-- TODO: split shell-header from protocol/ GADT per proto level
instance TezosBinary BlockHeader where
  parseBinary :: Parser BlockHeader
  parseBinary = (<?> "BlockHeader") $ BlockHeader
    <$> (parseBinary <?> "level")
    <*> (parseBinary <?> "proto")
    <*> (parseBinary <?> "predecessor")
    <*> (parseBinary <?> "timestamp")
    <*> (parseBinary <?> "validation_pass")
    <*> (parseBinary <?> "operations_hash")
    <*> (parseBinary <?> "fitness")
    <*> (parseBinary <?> "context")
    <*> (parseBinary <?> "priority")
    <*> (parseBinary <?> "proof_of_work_nonce")
    <*> (parseBinary <?> "seed_nonce_hash")

  encodeBinary bh = encodeBinary (_blockHeader_level bh)
                 <> encodeBinary (_blockHeader_proto bh)
                 <> encodeBinary (_blockHeader_predecessor bh)
                 <> encodeBinary (_blockHeader_timestamp bh)
                 <> encodeBinary (_blockHeader_validationPass bh)
                 <> encodeBinary (_blockHeader_operationsHash bh)
                 <> encodeBinary (_blockHeader_fitness bh)
                 <> encodeBinary (_blockHeader_context bh)
                 <> encodeBinary (_blockHeader_priority bh)
                 <> encodeBinary (_blockHeader_proofOfWorkNonce bh)
                 <> encodeBinary (_blockHeader_seedNonceHash bh)

