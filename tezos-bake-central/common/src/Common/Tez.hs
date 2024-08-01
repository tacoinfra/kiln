{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Tez where

import Data.Fixed (divMod')
import Data.Text (Text)
import qualified Data.Text as T

import Tezos.Types (Tez (..))

import ExtraPrelude

tez :: Tez -> Text
tez t = let (w, p, tz) = tez' t
         in w <> p <> tz

tez' :: Tez -> (Text, Text, Text)
tez' = tez'' False

tezPadded :: Tez -> (Text, Text, Text)
tezPadded = tez'' True

tez'' :: Bool -> Tez -> (Text, Text, Text)
tez'' pad (Tez n) = (T.pack wholes', padded, "ꜩ")
  where (wholes :: Integer, parts) = n `divMod'` 1
        wholes' = reverse $ f $ reverse $ show wholes
        parts' = T.dropWhileEnd (== '.')
                 $ T.dropAround (== '0')
                 $ tshow parts
        padded = T.pack . (T.unpack parts' &) $ if not pad then id else \case
          [] -> ".00"
          ['.', a] -> ['.', a, '0']
          as -> as
        f = \case
          (a0 : a1 : a2 : as) | as /= [] -> a0 : a1 : a2 : ',' : f as
          as -> as
