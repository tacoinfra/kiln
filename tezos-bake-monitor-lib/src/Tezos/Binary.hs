{-# LANGUAGE LambdaCase #-}
module Tezos.Binary where

import Control.Applicative (many)
import Data.Binary.Builder
import Data.Binary.Get
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

-- we all scream for ice cream
(<**) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g a)
x <** y = getCompose (Compose x <* Compose y)

