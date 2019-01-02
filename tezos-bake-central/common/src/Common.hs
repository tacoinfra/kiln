{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import qualified Cases
import qualified Data.Aeson as Aeson
import qualified Data.Map.Monoidal as MMap
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock (NominalDiffTime)
import qualified Text.URI as Uri
import Data.Maybe (mapMaybe)
import ExtraPrelude

nominalDiffTimeToSeconds :: NominalDiffTime -> Integer
nominalDiffTimeToSeconds n = numerator ratio `div` denominator ratio
  where
    ratio = toRational n

humanizeTimestamp :: Time.TimeZone -> Time.UTCTime -> Time.UTCTime -> Text
humanizeTimestamp tz now ts = if diff > 0 then futureMoment else humanizeDiffTime diff
  where
    diff = Time.diffUTCTime ts now
    dayOfWeek = case (Time.diffDays `on` Time.utctDay) ts now of
      0 -> "Today"
      1 -> "Tomorrow"
      _ -> "%A"
    futureMoment = T.pack $ Time.formatTime Time.defaultTimeLocale (dayOfWeek <> ", %-l:%M%P %Z") $ Time.utcToZonedTime tz ts

humanizeDiffTime :: Time.NominalDiffTime -> Text
humanizeDiffTime t = T.unwords elems <> " ago"
  where
    hms :: [Integer] -> Integer -> [Integer]
    hms (x:xs) n = (n `mod` x):hms xs (n `div` x)
    hms [] n = [n]

    totalseconds = nominalDiffTimeToSeconds t

    -- keep 2 elements if the first is a 1, otherwise take 1 element
    take2 (xy@(x, _y):xys)
      | x >= 2 = [xy]
      | otherwise = xy:take 1 xys
    take2 xys = take 2 $ xys

    elems = mapMaybe showElem
      $ take2
      $ dropWhile ((== 0) . fst)
      $ reverse
      $ zip (hms [60,60,24] (abs totalseconds)) "smhd"

    showElem (n, u)
      | n == 0 = Nothing
      | otherwise = Just $ T.pack (show n) <> T.singleton u

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds n = (numerator ratio * microsecondsInSecond) `div` denominator ratio
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

humanBytes :: Double -> Text
humanBytes n = tshow (round n' :: Int) <> u
  where
    (n' :: Double, u :: Text)
      | n >= 2^(40 :: Int) = (n / 2**40, "TB")
      | n >= 2^(30 :: Int) = (n / 2**30, "GB")
      | n >= 2^(20 :: Int) = (n / 2**20, "MB")
      | n >= 2^(10 :: Int) = (n / 2**10, "KB")
      | otherwise = (n, "B")
