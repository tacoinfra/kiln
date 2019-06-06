{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module Tezos.Micheline where

import Control.Applicative
import Data.Aeson (ToJSON, FromJSON, (.:), (.:?), (.!=), withObject, toEncoding, toJSON, parseJSON)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq

import Tezos.Base16ByteString
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



data Expression
   = Expression_Int !TezosWord64
   | Expression_String !Text
   | Expression_Bytes !(Base16ByteString ByteString)
   | Expression_Seq !(Seq (Expression))
   | Expression_Prim !(MichelinePrimAp)
  deriving (Eq, Ord, Show, Typeable)

data MichelinePrimAp = MichelinePrimAp
  { _michelinePrimAp_prim :: !MichelinePrimitive
  , _michelinePrimAp_args :: !(Seq Expression)
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
