{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Common.HeadTag where

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class ((=:))
import Reflex.Dom.Widget.Basic

import Static

headTag :: DomBuilder t m => m ()
headTag = do
  traverse_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ static @ "css/font-awesome.min.css"
    , static @ "semantic-ui/semantic.min.css"
    , static @ "css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank
