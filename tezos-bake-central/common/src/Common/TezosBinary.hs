{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Common.TezosBinary where

import Prelude hiding (take)

import Control.Monad
import Data.List (intercalate)
import Data.Proxy
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import GHC.Word
import GHC.Generics
import Data.Void

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq

import Focus.Schema (Json(..))

class TezosBinary a where
  parseBinary :: Parser a
  encodeBinary :: a -> ByteString -- lazy might be better?

eitherBinary :: TezosBinary a => String -> ByteString -> Either String a
eitherBinary hint x = let
    bakedEnough (Done _ r)        = Right r
    bakedEnough (Fail _ [] msg)   = Left msg
    bakedEnough (Fail _ ctxs msg) = Left (intercalate " > " ctxs ++ ": " ++ msg)
    bakedEnough _ = error "not supposed to be partial by this point"
  in case parse (parseBinary <?> "eitherBinary:"<>hint) x of
    Partial f -> bakedEnough (f mempty)
    done -> bakedEnough done

concatWithShift
  ::
  ( Integral x
  , Integral y
  , Num z , Bits z
  )
  => Int -> x -> y -> z
concatWithShift s x y = fromIntegral x `shift` s .|. fromIntegral y

unconcatWithShift
  ::
  ( Num x
  , Num y
  , Integral z, Bits z
  ) =>
  Int -> z -> (x , y)
unconcatWithShift s z = (fromIntegral (z `shift` negate s), fromIntegral z)

parseFixedByteString :: Int -> Parser BS.ByteString
parseFixedByteString = take

parseByteStringAll :: Parser BS.ByteString
parseByteStringAll = BS.pack <$> many' anyWord8

-- instance TezosBinary (LengthPrefixedBytes ByteString) where
parseLengthPrefixedByteString :: Parser BS.ByteString
parseLengthPrefixedByteString = do
  len <- parseBinary @ Int32
  parseFixedByteString $ fromIntegral len

encodeLengthPrefixedByteString :: ByteString -> ByteString
encodeLengthPrefixedByteString x = encodeBinary len <> x
  where
    len :: Int32 = fromInteger $ toInteger $ BS.length x

parserRecursiveLengthPrefixed :: Parser a -> Parser [a]
parserRecursiveLengthPrefixed p = do
  payload <- parseLengthPrefixedByteString
  let parseInnerResult loop = \case
        Fail _ ctxs msg -> error $ msg <> show ctxs
        Done _ r -> return r
        Partial f ->
          if loop
          then parseInnerResult False $ f (BS.pack [])
          else error "not supposed to get partial here"
  parseInnerResult True (parse (many' (p <?> "parserRecursiveLengthPrefixed")) payload)

parseEnum :: (Enum a, Bounded a) => Parser a
parseEnum = parseEnumWith (parseBinary @ Word8)

parseEnumWith :: forall a i. (Enum a, Bounded a, Integral i) => Parser i -> Parser a
parseEnumWith p = do
  tag' :: i <- p
  let tag = fromIntegral tag'
  when (tag < fromEnum (minBound :: a) || tag > fromEnum (maxBound :: a)) $ do
    fail "out of range"
  return $ toEnum $ tag

encodeEnum :: Enum a => a -> ByteString
encodeEnum = encodeEnumWith (encodeBinary @ Word8)

encodeEnumWith :: (Num i, Enum a) => (i -> ByteString) -> a -> ByteString
encodeEnumWith e = e . fromIntegral . fromEnum


parseTagged :: TezosBinary a => Word8 -> String
  -> (a -> b) -> Parser b
parseTagged tag hint ctor = word8 tag 
  >> (ctor <$> parseBinary) <?> hint

parseTagged2 :: (TezosBinary a, TezosBinary b) => Word8 -> String
  -> (a -> b -> c) -> Parser c
parseTagged2 tag hint ctor = word8 tag 
  >> (ctor <$> parseBinary <*> parseBinary) <?> hint

parseTagged3 :: (TezosBinary a, TezosBinary b, TezosBinary c) => Word8 -> String
  -> (a -> b -> c -> d) -> Parser d
parseTagged3 tag hint ctor = word8 tag
  >> (ctor <$> parseBinary <*> parseBinary <*> parseBinary) <?> hint

parseTagged4 :: (TezosBinary a, TezosBinary b, TezosBinary c, TezosBinary d) => Word8 -> String
  -> (a -> b -> c -> d -> e) -> Parser e
parseTagged4 tag hint ctor = word8 tag
  >> (ctor <$> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary) <?> hint

parseTagged5 :: (TezosBinary a, TezosBinary b, TezosBinary c, TezosBinary d, TezosBinary e) => Word8 -> String
  -> (a -> b -> c -> d -> e -> f) -> Parser f
parseTagged5 tag hint ctor = word8 tag
  >> (ctor <$> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary) <?> hint

parseTagged6 :: (TezosBinary a, TezosBinary b, TezosBinary c, TezosBinary d, TezosBinary e, TezosBinary f) => Word8 -> String
  -> (a -> b -> c -> d -> e -> f -> g) -> Parser g
parseTagged6 tag hint ctor = word8 tag
  >> (ctor <$> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary <*> parseBinary) <?> hint

instance (TezosBinary a, TezosBinary b) => TezosBinary (a, b) where
  parseBinary = ((,) <$> parseBinary <*> parseBinary ) <?> "(,)"
  encodeBinary (x, y) = encodeBinary x <> encodeBinary y

-- An instance for ByteString cannot be canonical, since there are two "good" instances,
-- a lenght prefixed one, which would get used regularly, and the "slurp the
-- rest of the buffer" one, useful for FromJSON instances. if you think you
-- need it, use one of the monomorphic parsers lying around in this module
-- instance TezosBinary ByteString where

newtype LengthPrefixed a = LengthPrefixed {unlengthPrefixed :: a}
  deriving (Eq, Ord, Show, Generic, Typeable, Functor, Foldable, Traversable)


instance TezosBinary (LengthPrefixed ByteString) where
  parseBinary = LengthPrefixed <$> parseLengthPrefixedByteString
  encodeBinary = encodeLengthPrefixedByteString . unlengthPrefixed

-- instance TezosBinary LBS.ByteString where
--   parseBinary = LBS.fromStrict <$> parseBinary
--   encodeBinary = encodeBinary . LBS.toStrict

instance TezosBinary Word8 where
  parseBinary = anyWord8
  encodeBinary x = BS.pack [x]

encodeBigEndian
  :: forall f a b z .
  ( TezosBinary a , Num a
  , TezosBinary b , Num b
  , Integral z , Bits z
  ) =>
  Int -> f a -> f b -> z -> ByteString
encodeBigEndian s _ _ z = encodeBinary a <> encodeBinary b
    where (a, b) :: (a, b) = unconcatWithShift s z


instance TezosBinary Word16 where
  parseBinary :: Parser Word16
  parseBinary = concatWithShift 8 <$> anyWord8 <*> anyWord8

  encodeBinary = encodeBigEndian 8 (Proxy @ Word8) (Proxy @ Word8)


instance TezosBinary Word32 where
  parseBinary :: Parser Word32
  parseBinary = concatWithShift 16 <$> parseBinary @ Word16 <*> parseBinary @ Word16

  encodeBinary = encodeBigEndian 16 (Proxy @ Word16) (Proxy @ Word16)

instance TezosBinary Word64 where
  parseBinary :: Parser Word64
  parseBinary = concatWithShift 32 <$> parseBinary @ Word32 <*> parseBinary @ Word32

  encodeBinary = encodeBigEndian 32 (Proxy @ Word32) (Proxy @ Word32)

instance TezosBinary Int8 where
  parseBinary :: Parser Int8
  parseBinary = fromIntegral <$> anyWord8

  encodeBinary = encodeBinary @ Word8 . fromIntegral

instance TezosBinary Int16 where
  parseBinary :: Parser Int16
  parseBinary = fromIntegral <$> parseBinary @ Word16

  encodeBinary = encodeBinary @ Word16 . fromIntegral

instance TezosBinary Int32 where
  parseBinary :: Parser Int32
  parseBinary = fromIntegral <$> parseBinary @ Word32

  encodeBinary = encodeBinary @ Word32 . fromIntegral

instance TezosBinary Int64 where
  parseBinary :: Parser Int64
  parseBinary = fromIntegral <$> parseBinary @ Word64

  encodeBinary = encodeBinary @ Word64 . fromIntegral

instance TezosBinary a => TezosBinary (Maybe a) where
  parseBinary :: Parser (Maybe a)
  parseBinary = (<?> "Maybe") $ do
    present <- anyWord8
    case present of
      0 -> return Nothing
      1 -> Just <$> parseBinary
      bad -> fail $ "bad tag in opt:" <> show bad

  encodeBinary Nothing = (encodeBinary @ Word8) 0
  encodeBinary (Just x) = (encodeBinary @ Word8) 1 <> encodeBinary x

instance TezosBinary a => TezosBinary [a] where
  parseBinary = parserRecursiveLengthPrefixed (parseBinary <?> "[]")

  encodeBinary xs = encodeLengthPrefixedByteString
                  $ BS.concat
                  $ encodeLengthPrefixedByteString . encodeBinary
                  <$> xs

instance TezosBinary a => TezosBinary (Seq a) where
  parseBinary = Seq.fromList <$> (parseBinary <?> "Seq")
  encodeBinary = encodeBinary . toList

instance TezosBinary UTCTime where
  parseBinary :: Parser UTCTime
  parseBinary = mkTime <$> ((parseBinary @ Word64) <?> "UTCTime")
    where
      mkTime :: Word64 -> UTCTime
      mkTime = posixSecondsToUTCTime . fromIntegral

  encodeBinary = encodeBinary . unmkTime
    where
      unmkTime :: UTCTime -> Word64
      unmkTime = floor . utcTimeToPOSIXSeconds


instance TezosBinary a => TezosBinary (Json a) where
  parseBinary = Json <$> (parseBinary <?> "Json")
  encodeBinary (Json a) = encodeBinary a

instance TezosBinary Bool where
  parseBinary = (0 /=) <$> anyWord8 <?> "bool"
  encodeBinary True = BS.pack [255]
  encodeBinary False = BS.pack [0]

instance TezosBinary Void where
  parseBinary = fail "Void"
  encodeBinary = absurd
