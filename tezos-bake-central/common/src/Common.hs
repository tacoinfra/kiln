{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Cases
import qualified Data.Aeson as Aeson
import qualified Data.Map.Monoidal as MMap
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock (NominalDiffTime)
import qualified Text.URI as Uri
import ExtraPrelude

nominalDiffTimeToSeconds :: NominalDiffTime -> Integer
nominalDiffTimeToSeconds n = numerator ratio * denominator ratio
  where
    ratio = toRational n

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds n = numerator ratio * (microsecondsInSecond `div` denominator ratio)
  where
    microsecondsInSecond = 10^(6 :: Integer)
    ratio = toRational n

curryMap :: (Eq a) => MonoidalMap (a, b) c -> MonoidalMap a (MonoidalMap b c)
curryMap = MMap.fromAscList . fmap (\((a, b), c) -> (a, MMap.singleton b c)) . MMap.toAscList

maybeSomething :: Foldable f => f a -> Maybe (f a)
maybeSomething as = if null as then Nothing else Just as

unixEpoch :: Time.UTCTime
unixEpoch = Time.UTCTime (Time.fromGregorian 1970 1 1) 0

uriHostPortPath :: Uri.URI -> Text
uriHostPortPath uri = auth <> path
  where
    auth = case Uri.uriAuthority uri of
      Left _ -> ""
      Right a -> Uri.unRText (Uri.authHost a) <> maybe "" (\p -> ":" <> tshow p) (Uri.authPort a)
    path = case Uri.uriPath uri of
      Nothing -> ""
      Just (_, pieces) -> T.intercalate "/" $ toList $ Uri.unRText <$> pieces

defaultTezosCompatJsonOptions :: Aeson.Options
defaultTezosCompatJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=) . tail
  , Aeson.constructorTagModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=)
  }
