{-# LANGUAGE LambdaCase #-}
module Tezos.Binary where

import Control.Applicative (many)
import Data.Binary.Builder
import Data.Binary.Get
import Data.Bits (Bits, (.&.), (.|.), bit, setBit, shift, testBit, zeroBits)
import Numeric.Natural (Natural)
import Data.Bool (Bool(..), bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const
import Data.Int
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Word
import qualified Data.Text
import qualified Data.Text.Encoding as TE

import Tezos.ShortByteString (ShortByteString, fromShort, toShort)

-- FIXME there are a million places integer overflow should be checked, just
-- grep for fromIntegral

class TezosBinary a where
  build :: a -> Builder
  build = getConst . put
  put :: a -> Const Builder ()
  put = Const . build
  get :: Get a

puts :: TezosBinary b => (a -> b) -> a -> Const Builder ()
puts = (put .)

encode :: TezosBinary a => a -> BS.ByteString
encode = LBS.toStrict . toLazyByteString . build

decode :: TezosBinary a => BS.ByteString -> a
decode x = runGet (isolate (BS.length x) get) $ LBS.fromChunks $ pure x

decodeEither :: TezosBinary a => BS.ByteString -> Either String a
decodeEither x = either (\(_,_,r) -> Left r) (\(_,_,r) -> Right r) $ runGetOrFail (isolate (BS.length x) get) $ LBS.fromChunks $ pure x

class TezosUnsignedBinary a where
  putUnsigned :: a -> Const Builder ()
  getUnsigned :: Get a

newtype ToSign a = ToSign { unToSign :: a }
instance TezosUnsignedBinary a => TezosBinary (ToSign a) where
  put (ToSign x) = putUnsigned x
  get = ToSign <$> getUnsigned

newtype DynamicSize a = DynamicSize { unDynamicSize :: a }
instance TezosBinary a => TezosBinary (DynamicSize a) where
  build (DynamicSize a) = putWord32be (fromIntegral $ LBS.length $ toLazyByteString $ build a) <> build a
  get = do
    len <- fromIntegral <$> getWord32be
    DynamicSize <$> isolate len get

instance TezosBinary Word8 where
  build = singleton
  get = getWord8

instance TezosBinary Word16 where
  build = putWord16be
  get = getWord16be

instance TezosBinary Word32 where
  build = putWord32be
  get = getWord32be

instance TezosBinary Int32 where
  build = putInt32be
  get = getInt32be

instance TezosBinary Int64 where
  build = putInt64be
  get = getInt64be

readZ :: (Num a, Bits a) => Int -> a -> Get a
readZ offset n = do
  b <- getWord8
  if (b == 0) && (offset > 0) then fail "trailing zero" else pure ()
  let n' = (fromIntegral (b .&. 0x7f) `shift` offset) .|. n
  if b `testBit` 7 then readZ (offset + 7) n' else pure n'

writeZ :: (Integral a, Ord a, Bits a) => Int -> a -> Builder
writeZ offset n =
  if n < bit (7 - offset) then singleton $ fromIntegral $ n `shift` offset
    else singleton (fromIntegral (((n `shift` offset) .&. 0x7f) `setBit` 7)) <> writeZ (offset - 7) n

instance TezosBinary Natural where
  build = writeZ 0
  get = readZ 0 0

instance TezosBinary Integer where
  build n =
    let signBit = if n < 0 then bit 6 else zeroBits
        ab = abs n
    in
    if ab < 0x40 then singleton (fromIntegral ab .|. signBit)
      else singleton (fromIntegral (ab .&. 0x3f) .|. signBit .|. bit 7) <> writeZ 6 ab
  get = do
    b <- getWord8
    n <- if b `testBit` 7 then readZ 6 (fromIntegral $ b .&. 0x3f) else pure (fromIntegral $ b .&. 0x3f)
    pure $ if b `testBit` 6 then negate n else n

instance TezosBinary Bool where
  put = puts (bool 0x00 0xff :: Bool -> Word8)
  get = getWord8 >>= \case
    0x00 -> pure False
    0xff -> pure True
    _ -> fail "invalid encoding for Bool"

instance TezosBinary x => TezosBinary (Maybe x) where
  put = \case
    Nothing -> put False
    Just x -> put True <* put x
  get = get >>= bool (pure Nothing) (Just <$> get)

instance TezosBinary UTCTime where
  put = puts (round . utcTimeToPOSIXSeconds :: UTCTime -> Int64)
  get = (posixSecondsToUTCTime . fromIntegral :: Int64 -> UTCTime) <$> get

instance TezosBinary BS.ByteString where
  build = fromByteString
  get = LBS.toStrict <$> getRemainingLazyByteString

instance TezosBinary ShortByteString where
  put = puts fromShort
  get = toShort <$> get

instance TezosBinary a => TezosBinary (Seq a) where
  build = foldMap build
  put = traverse_ put
  get = Seq.fromList <$> many get

instance TezosBinary Data.Text.Text where
  build = build . TE.encodeUtf8
  put = put . TE.encodeUtf8
  get = fmap TE.decodeUtf8' get >>= \case
    Left err -> fail $ show err
    Right answer -> pure answer

-- we all scream for ice cream
(<**) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g a)
x <** y = getCompose (Compose x <* Compose y)

