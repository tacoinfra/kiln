module Common where

import Data.AppendMap(AppendMap)
import qualified Data.AppendMap as AMap
import Data.Text (Text)
import qualified Data.Text as T


tshow :: Show a => a -> Text
tshow = T.pack . show

whenJust :: (Applicative m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing _ = pure mempty
whenJust (Just x) f = f x

whenM :: (Applicative m, Monoid b) => Bool -> m b -> m b
whenM x true = if x then true else pure mempty

curryMap :: (Eq a) => AppendMap (a, b) c -> AppendMap a (AppendMap b c)
curryMap = AMap.fromAscList . fmap (\((a, b), c) -> (a, AMap.singleton b c)) . AMap.toAscList
-- uncurryMap :: (Eq a, Eq b) => AppendMap a (AppendMap b c) -> AppendMap (a, b) c
-- uncurryMap = AMap.fromAscList . _ . AMap.toAscList
