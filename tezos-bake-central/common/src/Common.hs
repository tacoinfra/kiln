{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as AMap
import Data.Foldable (toList)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.URI as Uri

tshow :: Show a => a -> Text
tshow = T.pack . show

whenJust :: (Applicative m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing _ = pure mempty
whenJust (Just x) f = f x

curryMap :: (Eq a) => AppendMap (a, b) c -> AppendMap a (AppendMap b c)
curryMap = AMap.fromAscList . fmap (\((a, b), c) -> (a, AMap.singleton b c)) . AMap.toAscList

maybeSomething :: Foldable f => f a -> Maybe (f a)
maybeSomething as = if null as then Nothing else Just as

uriHostPortPath :: Uri.URI -> Text
uriHostPortPath uri = auth <> path
  where
    auth = case Uri.uriAuthority uri of
      Left _ -> ""
      Right a -> Uri.unRText (Uri.authHost a) <> maybe "" (\p -> ":" <> tshow p) (Uri.authPort a)
    path = case Uri.uriPath uri of
      Nothing -> ""
      Just (_, pieces) -> T.intercalate "/" $ toList $ Uri.unRText <$> pieces
