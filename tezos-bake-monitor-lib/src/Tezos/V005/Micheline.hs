{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Tezos.V005.Micheline
  ( module Tezos.V005.Micheline
  , module Old
  ) where
  

-- TODO: https://tezos.gitlab.io/master/protocols/005_babylon.html#id4

import Control.Applicative
import Data.Aeson (ToJSON, FromJSON, (.:), (.:?), (.!=), withObject, toEncoding, toJSON, parseJSON)
import Data.ByteString (ByteString)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Data.Typeable
import Data.Word (Word8)
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq

import Tezos.V005.Base16ByteString
import qualified Tezos.V005.Binary as B
import Tezos.V005.Json

import Tezos.V004.Micheline as Old (michelsonV1Enum)

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
  "EMPTY_MAP", "CREATE_CONTRACT", "HASH_KEY", "ISNAT", "code", "AND", "APPLY"
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
  -- FIXME should be annotations here
  } deriving (Eq, Ord, Show, Typeable)

instance FromJSON MichelinePrimAp where
  parseJSON = withObject "Prim" $ \v -> MichelinePrimAp
    <$> v .: "prim"
    <*> v .:? "args" .!= mempty

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

instance B.TezosBinary Expression where
  put = \case
    Expression_Seq xs -> B.put @Word8 2 *> B.put (B.DynamicSize xs)
    Expression_Prim (MichelinePrimAp prim args) -> case args of
      Empty -> B.put @Word8 3 *> B.put prim
      arg1 :<| Empty -> B.put @Word8 5 *> B.put prim *> B.put arg1
      arg1 :<| (arg2 :<| Empty) -> B.put @Word8 7 *> B.put prim *> B.put arg1 *> B.put arg2
      _ -> B.put @Word8 9 *> B.put prim *> B.put args *> B.put @(B.DynamicSize Text) (B.DynamicSize "")
    Expression_String x -> B.put @Word8 1 *> B.put (B.DynamicSize x)
    Expression_Int x -> B.put @Word8 0 *> B.put @Integer (fromIntegral x)
    Expression_Bytes x -> B.put @Word8 10 *> B.put (B.DynamicSize x)
  get = B.get @Word8 >>= \case
    0 -> Expression_Int . fromIntegral <$> B.get @Integer
    1 -> Expression_String . B.unDynamicSize <$> B.get
    2 -> Expression_Seq . B.unDynamicSize <$> B.get
    3 -> Expression_Prim . flip MichelinePrimAp Seq.Empty <$> B.get
    4 -> Expression_Prim . flip MichelinePrimAp Seq.Empty <$> B.get <* B.get @(B.DynamicSize Text) -- FIXME don't toss annots
    5 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> (Seq.singleton <$> B.get))
    6 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> (Seq.singleton <$> B.get) <* B.get @(B.DynamicSize Text)) -- FIXME don't toss annots
    7 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> Seq.replicateA 2 B.get)
    8 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> Seq.replicateA 2 B.get <* B.get @(B.DynamicSize Text)) -- FIXME don't toss annots
    9 -> Expression_Prim <$> (MichelinePrimAp <$> B.get <*> B.get <* B.get @(B.DynamicSize Text)) -- FIXME don't toss annots
    10 -> Expression_Bytes . B.unDynamicSize <$> B.get
    _ -> fail "invalid Micheline expression tag"
