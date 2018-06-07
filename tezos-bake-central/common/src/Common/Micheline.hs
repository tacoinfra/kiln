{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Common.Micheline where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Typeable
import GHC.Generics
import GHC.Word

import Common.TezosBinary

type Micheline_V1_Prim = Word8

data Node p
   = Node_Int (LengthPrefixed ByteString)
   | Node_String (LengthPrefixed ByteString)
   | Node_Prim p [Node p] (Maybe (LengthPrefixed ByteString))
   | Node_Seq (Seq (Node p)) -- lib_micheline/micheline.ml always throws away annot on seq (Maybe (LengthPrefixed ByteString))
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable, Traversable)

p0_ :: p -> Node p
p0_ v             = Node_Prim v [          ] Nothing
p0a :: p -> LengthPrefixed ByteString -> Node p
p0a v           a = Node_Prim v [          ] (Just a)
p1_ :: p -> Node p -> Node p
p1_ v arg1        = Node_Prim v [arg1      ] Nothing
p1a :: p -> Node p -> LengthPrefixed ByteString -> Node p
p1a v arg1      a = Node_Prim v [arg1      ] (Just a)
p2_ :: p -> Node p -> Node p -> Node p
p2_ v arg1 arg2   = Node_Prim v [arg1, arg2] Nothing
p2a :: p -> Node p -> Node p -> LengthPrefixed ByteString -> Node p
p2a v arg1 arg2 a = Node_Prim v [arg1, arg2] (Just a)

app :: p -> [Node p] -> Maybe (LengthPrefixed ByteString) -> Node p
app prim args a = Node_Prim prim args a

instance TezosBinary p => TezosBinary (Node p) where
  parseBinary = parseTagged  0 "int" Node_Int
        `mplus` parseTagged  1 "string" Node_String
        `mplus` parseTagged  2 "seq" Node_Seq
        `mplus` parseTagged  3 "prim ()" p0_
        `mplus` parseTagged2 4 "prim () a" p0a
        `mplus` parseTagged2 5 "prim (x)" p1_
        `mplus` parseTagged3 6 "prim (x) a" p1a
        `mplus` parseTagged3 7 "prim (x,y)" p2_
        `mplus` parseTagged4 8 "prim (x,y) a" p2a
        `mplus` parseTagged3 9 "prim (...)" app

  encodeBinary = \case
    Node_Int x                        -> encodeBinary (0 :: Word8) <> encodeBinary x
    Node_String x                     -> encodeBinary (1 :: Word8) <> encodeBinary x
    Node_Seq x                        -> encodeBinary (2 :: Word8) <> encodeBinary x
    Node_Prim p [] Nothing            -> encodeBinary (3 :: Word8) <> encodeBinary p
    Node_Prim p [] (Just a)           -> encodeBinary (4 :: Word8) <> encodeBinary p                                           <> encodeBinary a
    Node_Prim p [arg1] Nothing        -> encodeBinary (5 :: Word8) <> encodeBinary p <> encodeBinary arg1
    Node_Prim p [arg1] (Just a)       -> encodeBinary (6 :: Word8) <> encodeBinary p <> encodeBinary arg1                      <> encodeBinary a
    Node_Prim p [arg1, arg2] Nothing  -> encodeBinary (7 :: Word8) <> encodeBinary p <> encodeBinary arg1 <> encodeBinary arg2
    Node_Prim p [arg1, arg2] (Just a) -> encodeBinary (8 :: Word8) <> encodeBinary p <> encodeBinary arg1 <> encodeBinary arg2 <> encodeBinary a
    Node_Prim p args annot            -> encodeBinary (9 :: Word8) <> encodeBinary p <> encodeBinary args <> encodeBinary annot
