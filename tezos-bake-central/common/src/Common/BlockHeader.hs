{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Common.BlockHeader where

import qualified Data.Sequence as Seq
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Int
import GHC.Word
import Data.Bits

import Data.Attoparsec.ByteString

import Common.Schema
import Data.Semigroup

import Data.Time
import Data.Time.Clock.POSIX


concatWithShift
  ::
  ( Integral x
  , Integral y
  , Num z , Bits z
  )
  => Int -> x -> y -> z
concatWithShift s x y = fromIntegral x `shift` s .|. fromIntegral y

parseWord16LE :: Parser Word16
parseWord16LE = concatWithShift 8 <$> anyWord8 <*> anyWord8

parseWord32LE :: Parser Word32
parseWord32LE = concatWithShift 16 <$> parseWord16LE <*> parseWord16LE

parseWord64LE :: Parser Word64
parseWord64LE = concatWithShift 32 <$> parseWord32LE <*> parseWord32LE

parseInt8 :: Parser Int8
parseInt8 = fromIntegral <$> anyWord8

parseInt16LE :: Parser Int16
parseInt16LE = fromIntegral <$> parseWord16LE

parseInt32LE :: Parser Int32
parseInt32LE = fromIntegral <$> parseWord32LE

parseFixedByteString :: Int -> Parser BS.ByteString
parseFixedByteString len = BS.pack <$> count len anyWord8

parseLengthPrefixedByteString :: Parser BS.ByteString
parseLengthPrefixedByteString = do
  len <- parseInt32LE
  parseFixedByteString $ fromIntegral len

parseOpt :: Parser a -> Parser (Maybe a)
parseOpt p = do
  present <- anyWord8
  case present of
    0 -> return Nothing
    1 -> Just <$> p
    bad -> fail $ "bad tag in opt:" <> show bad

parseHash :: Parser BS.ByteString
parseHash = parseFixedByteString 32

parseFitness :: Parser Fitness
parseFitness = do
  payload <- parseLengthPrefixedByteString
  let thingResult loop = \case
        Fail _ ctxs msg -> foldl' (<?>) (fail msg) ctxs
        Done _ r -> return $ Fitness $ Seq.fromList $ fmap Base16ByteString r
        Partial f ->
          if loop
          then thingResult False $ f (BS.pack [])
          else error "not supposed to get partial here"
  thingResult True $ parse (many' parseLengthPrefixedByteString) payload

parseTimestamp :: Parser UTCTime
parseTimestamp = mkTime <$> parseWord64LE
  where
    mkTime :: Word64 -> UTCTime
    mkTime = posixSecondsToUTCTime . fromIntegral

-- TODO:  generate this automatically.;  see tezos/binary-description branch...
-- TODO: split shell-header from protocol/ GADT per proto level
parseBlock :: Parser BlockHeader
parseBlock = BlockHeader
  <$> (parseInt32LE <?> "level")
  <*> (anyWord8 <?> "proto")
  <*> (parseHash <?> "predecessor")
  <*> (parseTimestamp <?> "timestamp")
  <*> (anyWord8 <?> "validation_pass")
  <*> (parseHash <?> "operations_hash")
  <*> (parseFitness <?> "fitness")
  <*> (parseHash <?> "context")
  <*> (parseWord16LE <?> "priority")
  <*> (parseWord64LE <?> "proof_of_work_nonce")
  <*> (parseOpt parseLengthPrefixedByteString <?> "seed_nonce_hash")

-- TODO: handle parsing errors
blockLevel :: Event BakedEvent -> Int
blockLevel evt = fromIntegral ( _blockHeader_level ( either error id ( eitherResult ( parse (parseBlock) ( unbase16ByteString ( _bakedEvent_signedHeader ( _event_detail evt)))))))
