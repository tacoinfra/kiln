module Common where

import Data.Text (Text)
import qualified Data.Text as T


tshow :: Show a => a -> Text
tshow = T.pack . show

whenJust :: (Applicative m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing f = pure mempty
whenJust (Just x) f = f x
