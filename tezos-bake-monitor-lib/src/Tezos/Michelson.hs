{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Tezos.Michelson where

import qualified Data.ByteString as BS
import Data.Sequence (Seq(..))
import qualified Data.Text as T
import Prelude hiding (pattern Left, pattern Right)

import Tezos.Base16ByteString (Base16ByteString(..))
import qualified Tezos.Binary as B
import Tezos.Micheline (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

pattern Prim :: T.Text -> Seq Expression -> Expression
pattern Prim p a = Expression_Prim (MichelinePrimAp (MichelinePrimitive p) a)

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
