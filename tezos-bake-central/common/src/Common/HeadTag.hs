{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.HeadTag where

import Data.Foldable (for_, traverse_)
import Data.Semigroup ((<>))
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class ((=:))
import Reflex.Dom.Widget.Basic
import Text.URI as Uri

headTag :: DomBuilder t m => Maybe Uri.URI -> m ()
headTag baseUri = do
  for_ baseUri $ \uri ->
    elAttr "base" ("href"=:Uri.render uri) blank
  traverse_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ "/css/font-awesome.min.css"
    , "/semantic-ui/semantic.min.css"
    , "/css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank
