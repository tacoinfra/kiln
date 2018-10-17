{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.Aeson as Aeson
import qualified Cases
import Data.Foldable (toList)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.URI as Uri
import qualified Data.Time as Time

tshow :: Show a => a -> Text
tshow = T.pack . show

whenJust :: (Applicative m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing _ = pure mempty
whenJust (Just x) f = f x

whenM :: (Applicative m, Monoid b) => Bool -> m b -> m b
whenM x true = if x then true else pure mempty

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
