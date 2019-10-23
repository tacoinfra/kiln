{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Tezos.V005.Michelson where

import qualified Data.ByteString as BS
import Data.Sequence (Seq(..))
import qualified Data.Text as T
import Prelude

import Tezos.Common.Base16ByteString (Base16ByteString(..))
import qualified Tezos.Common.Binary as B
import Tezos.V005.Micheline (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))
import Tezos.V005.Contract

pattern Prim :: T.Text -> Seq Expression -> Expression
pattern Prim p a <- Expression_Prim (MichelinePrimAp (MichelinePrimitive p) a _)
  where Prim p a = Expression_Prim (MichelinePrimAp (MichelinePrimitive p) a Empty)

pattern Prim0 :: T.Text -> Expression
pattern Prim0 p = Prim p Empty

pattern Prim1 :: T.Text -> Expression -> Expression
pattern Prim1 p a = Prim p (a :<| Empty)

pattern Prim2 :: T.Text -> Expression -> Expression -> Expression
pattern Prim2 p a b = Prim p (a :<| b :<| Empty)

pattern Pair :: Expression -> Expression -> Expression
pattern Pair a b = Prim2 "Pair" a b

pattern Left :: Expression -> Expression
pattern Left l = Prim1 "Left" l

pattern Right :: Expression -> Expression
pattern Right r = Prim1 "Right" r

pattern Int :: Integral a => a -> Expression
pattern Int x <- Expression_Int (fromIntegral -> x) where
  Int x = Expression_Int $ fromIntegral x

pattern Bytes :: BS.ByteString -> Expression
pattern Bytes x = Expression_Bytes (Base16ByteString x)

pattern AsBytes :: B.TezosBinary a => a -> Expression
pattern AsBytes x = Bytes (B.TezosBinary x)

class FromMicheline a where
  fromMicheline :: Expression -> Either String a

instance FromMicheline Expression where
  fromMicheline = Prelude.Right

mapEitherToString :: Show a => Either a b -> Either String b
mapEitherToString (Prelude.Left a) = Prelude.Left (show a)
mapEitherToString (Prelude.Right a) = Prelude.Right a

instance FromMicheline ContractId where
  fromMicheline (Expression_String a) = mapEitherToString $ tryReadContractIdText a
  fromMicheline (Expression_Bytes (Base16ByteString a)) = B.decodeEither a
  fromMicheline _ = Prelude.Left "Unrecognized type when decoding address"

class ToMicheline a where
  toMicheline :: a -> Expression

instance ToMicheline Expression where
  toMicheline = id

instance ToMicheline ContractId where
  toMicheline = AsBytes
