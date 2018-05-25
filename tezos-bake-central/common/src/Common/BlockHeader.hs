{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Common.BlockHeader where

import qualified Data.ByteString as BS
import Data.Int
import GHC.Word

import Data.Attoparsec.ByteString

import Data.Semigroup

import Data.Time

import Common.Fitness
import Data.Typeable
import GHC.Generics
import Common.TezosBinary
import Common.TaggedHash

-- TODO: split this into ShellHeader/AlphaProtoHeader/etc
data BlockHeader = BlockHeader
  { _blockHeader_level :: Int32
  , _blockHeader_proto :: Word8
  , _blockHeader_predecessor :: BlockHash -- BS.ByteString
  , _blockHeader_timestamp :: UTCTime
  , _blockHeader_validationPass :: Word8
  , _blockHeader_operationsHash :: OperationListListHash -- BS.ByteString
  , _blockHeader_fitness :: Fitness
  , _blockHeader_context :: ContextHash
  , _blockHeader_priority :: Word16
  , _blockHeader_proofOfWorkNonce :: Word64
  , _blockHeader_seedNonceHash :: Maybe BS.ByteString
  }
  deriving (Show, Eq, Ord, Typeable, Generic)



-- parseHash :: Parser BS.ByteString
-- parseHash = parseFixedByteString 32
-- 

-- TODO:  generate this automatically.;  see tezos/binary-description branch...
-- TODO: split shell-header from protocol/ GADT per proto level
instance TezosBinary BlockHeader where
  parseBinary :: Parser BlockHeader
  parseBinary = BlockHeader
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

