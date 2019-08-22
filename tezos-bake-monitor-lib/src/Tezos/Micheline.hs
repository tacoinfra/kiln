{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable #-}

module Tezos.Micheline where

import Control.Applicative
import Control.Monad.Fail (MonadFail)
import Data.Aeson (ToJSON, FromJSON, (.:), (.:?), (.!=), withObject, toEncoding, toJSON, parseJSON, withText)
import Data.Binary.Get (Get)
import Data.Binary.Builder (Builder)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Word (Word8)
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq

import Tezos.Base16ByteString
import qualified Tezos.Binary as B
import Tezos.Json


newtype MichelinePrimitive = MichelinePrimitive Text
  deriving (Eq, Ord, Show, Typeable, ToJSON, FromJSON)

michelineV1Primitive :: Seq Text
michelineV1Primitive = Seq.fromList [
  "ADD", "LE", "UPDATE", "unit", "string", "COMPARE", "LAMBDA", "LOOP", "Elt",
  "IMPLICIT_ACCOUNT", "NONE", "signature", "set", "mutez", "BLAKE2B", "SHA256",
  "ITER", "bool", "MAP", "UNIT", "DIP", "PACK", "pair", "SIZE", "Right", "map",
  "IF_CONS", "LSR", "SET_DELEGATE", "storage", "XOR", "CDR", "TRANSFER_TOKENS",
  "SOME", "False", "SHA512", "CHECK_SIGNATURE", "BALANCE", "lambda",
  "operation", "EMPTY_SET", "SWAP", "MEM", "RIGHT", "CONTRACT", "or", "CONCAT",
  "nat", "bytes", "Unit", "Some", "UNPACK", "NOT", "LEFT", "timestamp",
  "AMOUNT", "DROP", "ABS", "contract", "GE", "PUSH", "LT", "address", "NEQ",
  "NEG", "None", "CONS", "EXEC", "NIL", "CAST", "MUL", "ADDRESS", "EDIV",
  "STEPS_TO_QUOTA", "SUB", "INT", "SOURCE", "CAR", "CREATE_ACCOUNT", "LSL",
  "OR", "IF_NONE", "SELF", "IF", "Left", "int", "big_map", "SENDER", "option",
  "DUP", "EQ", "NOW", "key_hash", "GET", "list", "key", "True", "GT",
  "parameter", "IF_LEFT", "FAILWITH", "PAIR", "LOOP_LEFT", "Pair", "RENAME",
  "EMPTY_MAP", "CREATE_CONTRACT", "HASH_KEY", "ISNAT", "code", "AND"
  ]

michelsonV1Enum :: Seq Text
michelsonV1Enum = Seq.fromList [
  "parameter", "storage", "code", "False", "Elt", "Left", "None", "Pair",
  "Right", "Some", "True", "Unit", "PACK", "UNPACK", "BLAKE2B", "SHA256",
  "SHA512", "ABS", "ADD", "AMOUNT", "AND", "BALANCE", "CAR", "CDR",
  "CHECK_SIGNATURE", "COMPARE", "CONCAT", "CONS", "CREATE_ACCOUNT",
  "CREATE_CONTRACT", "IMPLICIT_ACCOUNT", "DIP", "DROP", "DUP", "EDIV",
  "EMPTY_MAP", "EMPTY_SET", "EQ", "EXEC", "FAILWITH", "GE", "GET", "GT",
  "HASH_KEY", "IF", "IF_CONS", "IF_LEFT", "IF_NONE", "INT", "LAMBDA",
  "LE", "LEFT", "LOOP", "LSL", "LSR", "LT", "MAP", "MEM", "MUL", "NEG",
  "NEQ", "NIL", "NONE", "NOT", "NOW", "OR", "PAIR", "PUSH", "RIGHT",
  "SIZE", "SOME", "SOURCE", "SENDER", "SELF", "STEPS_TO_QUOTA", "SUB",
  "SWAP", "TRANSFER_TOKENS", "SET_DELEGATE", "UNIT", "UPDATE", "XOR",
  "ITER", "LOOP_LEFT", "ADDRESS", "CONTRACT", "ISNAT", "CAST", "RENAME",
  "bool", "contract", "int", "key", "key_hash", "lambda", "list", "map",
  "big_map", "nat", "option", "or", "pair", "set", "signature", "string",
  "bytes", "mutez", "timestamp", "unit", "operation", "address", "SLICE"
  ]

instance B.TezosBinary MichelinePrimitive where
  build (MichelinePrimitive p) = case Seq.elemIndexL p michelsonV1Enum of
    Nothing -> error "unknown Michelson/Micheline primitive"
    Just ix -> B.build @Word8 (fromIntegral ix)
  get = B.get @Word8 >>= \ix -> case Seq.lookup (fromIntegral ix) michelsonV1Enum of
    Nothing -> fail "unknown Michelson/Micheline opcode"
    Just str -> pure $ MichelinePrimitive str

data Expression
   = Expression_Int !TezosWord64 -- FIXME this should be Integer!!
   | Expression_String !Text
   | Expression_Bytes !(Base16ByteString ByteString)
   | Expression_Seq !(Seq Expression)
   | Expression_Prim !MichelinePrimAp
  deriving (Eq, Ord, Show, Typeable)

data MichelinePrimAp = MichelinePrimAp
  { _michelinePrimAp_prim :: !MichelinePrimitive
  , _michelinePrimAp_args :: !(Seq Expression)
  , _michelinePrimAp_annots :: !(Seq Annotation)
  } deriving (Eq, Ord, Show, Typeable)

data Annotation
  = Annotation_Type !Text
  | Annotation_Variable !Text
  | Annotation_Field !Text
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON MichelinePrimAp where
  parseJSON = withObject "Prim" $ \v -> MichelinePrimAp
    <$> v .: "prim"
    <*> v .:? "args" .!= mempty
    <*> v .:? "annots" .!= mempty

annotFromText :: MonadFail m => Text -> m Annotation
annotFromText txt = case T.uncons txt of
  Just (':', t) -> pure $ Annotation_Type t
  Just ('@', t) -> pure $ Annotation_Variable t
  Just ('%', t) -> pure $ Annotation_Field t
  _ -> fail "Unknown annotation type"
annotToText :: Annotation -> Text
annotToText = \case
  Annotation_Type n -> T.cons ':' n
  Annotation_Variable n -> T.cons '@' n
  Annotation_Field n -> T.cons '%' n

instance FromJSON Annotation where
  parseJSON = withText "Annotation" $ annotFromText

instance ToJSON Annotation where
  toJSON = toJSON . annotToText

concat <$> traverse (Aeson.deriveToJSON tezosJsonOptions)
  [ ''MichelinePrimAp
  ]

instance FromJSON Expression where
  parseJSON v = Expression_Seq <$> parseJSON v
            <|> Expression_Prim <$> parseJSON v
            <|> Expression_String <$> withObject "Expression_String" (.: "string") v
            <|> Expression_Int <$> withObject "Expression_Int" (.: "int") v
            <|> Expression_Bytes <$> withObject "Expression_Bytes" (.: "bytes") v


instance ToJSON Expression where
  toJSON (Expression_Seq xs) = toJSON xs
  toJSON (Expression_Prim xs) = toJSON xs
  toJSON (Expression_String x) = Aeson.Object (HashMap.singleton "string" $ toJSON x)
  toJSON (Expression_Int x) = Aeson.Object (HashMap.singleton "int" $ toJSON x)
  toJSON (Expression_Bytes x) = Aeson.Object (HashMap.singleton "bytes" $ toJSON x)

  toEncoding (Expression_Seq xs) = toEncoding xs
  toEncoding (Expression_Prim xs) = toEncoding xs
  toEncoding (Expression_String x) = Aeson.pairs ( Aeson.pair "string" ( toEncoding x ))
  toEncoding (Expression_Int x) = Aeson.pairs ( Aeson.pair "int" ( toEncoding x ))
  toEncoding (Expression_Bytes x) = Aeson.pairs ( Aeson.pair "bytes" ( toEncoding x ))

-- src/proto_002_PsYLVpVv/lib_protocol/src/script_tc_errors_registration.ml:48:        (dft "annots" (list string) [])))
-- src/proto_002_PsYLVpVv/lib_protocol/src/script_tc_errors_registration.ml:116:                (dft "expectedPrimitiveNames" (list prim_encoding) [])

putAnnotationSeq :: Seq Annotation -> Const Builder ()
putAnnotationSeq = B.put . B.DynamicSize . T.unwords . toList . fmap annotToText
getAnnotationSeq :: Get (Seq Annotation)
getAnnotationSeq = sequence . (fmap annotFromText . Seq.fromList . T.words . B.unDynamicSize) =<< B.get @(B.DynamicSize Text)

instance B.TezosBinary Expression where
  put = \case
    Expression_Seq xs -> B.put @Word8 2 *> B.put (B.DynamicSize xs)
    Expression_Prim (MichelinePrimAp prim args annots) -> case (args, annots) of
      (Empty, Empty) -> B.put @Word8 3 *> B.put prim
      (Empty, _) -> B.put @Word8 4 *> B.put prim *> putAnnotationSeq annots
      (arg1 :<| Empty, Empty) -> B.put @Word8 5 *> B.put prim *> B.put arg1
      (arg1 :<| Empty, _) -> B.put @Word8 6 *> B.put prim *> B.put arg1 *> putAnnotationSeq annots
      (arg1 :<| (arg2 :<| Empty), Empty) -> B.put @Word8 7 *> B.put prim *> B.put arg1 *> B.put arg2
      (arg1 :<| (arg2 :<| Empty), _) -> B.put @Word8 8 *> B.put prim *> B.put arg1 *> B.put arg2 *> putAnnotationSeq annots
      _ -> B.put @Word8 9 *> B.put prim *> B.put args *> putAnnotationSeq annots
    Expression_String x -> B.put @Word8 1 *> B.put (B.DynamicSize x)
    Expression_Int x -> B.put @Word8 0 *> B.put @Integer (fromIntegral x)
    Expression_Bytes x -> B.put @Word8 10 *> B.put (B.DynamicSize x)
  get = B.get @Word8 >>= \case
    0 -> Expression_Int . fromIntegral <$> B.get @Integer
    1 -> Expression_String . B.unDynamicSize <$> B.get
    2 -> Expression_Seq . B.unDynamicSize <$> B.get
    3 -> Expression_Prim . (\pn -> MichelinePrimAp pn Seq.Empty Seq.Empty) <$> B.get
    4 -> Expression_Prim <$> (flip MichelinePrimAp Seq.Empty <$> B.get <*> getAnnotationSeq)
    5 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> (Seq.singleton <$> B.get) <*> (pure Seq.empty))
    6 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> (Seq.singleton <$> B.get) <*> getAnnotationSeq)
    7 -> Expression_Prim <$> ((\n a->MichelinePrimAp n a Seq.empty) <$> B.get <*> Seq.replicateA 2 B.get)
    8 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> Seq.replicateA 2 B.get <*> getAnnotationSeq)
    9 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> B.get <*> getAnnotationSeq)
    10 -> Expression_Bytes . B.unDynamicSize <$> B.get
    _ -> fail "invalid Micheline expression tag"
