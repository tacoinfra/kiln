{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.HeadTag where

import Data.Foldable (for_, traverse_)
import Data.Semigroup ((<>))
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class ((=:))
import Reflex.Dom.Widget.Basic

import Obelisk.Generated.Static

headTag :: DomBuilder t m => m ()
headTag = do
  let favicons =
        [ ("16x16",   $(static "images/favicons/favicon-16.png"))
        , ("32x32",   $(static "images/favicons/favicon-32.png"))
        , ("57x57",   $(static "images/favicons/favicon-57.png"))
        , ("76x76",   $(static "images/favicons/favicon-76.png"))
        , ("96x96",   $(static "images/favicons/favicon-96.png"))
        , ("120x120", $(static "images/favicons/favicon-120.png"))
        , ("128x128", $(static "images/favicons/favicon-128.png"))
        , ("144x144", $(static "images/favicons/favicon-144.png"))
        , ("152x152", $(static "images/favicons/favicon-152.png"))
        , ("180x180", $(static "images/favicons/favicon-180.png"))
        , ("228x228", $(static "images/favicons/favicon-228.png"))
        ]

  traverse_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ $(static "css/font-awesome.min.css")
    , $(static "semantic-ui/semantic.min.css")
    , $(static "icons/style.css")
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank
  el "title" $ text "Kiln"

  for_ favicons $ \(size,href) ->
    flip (elAttr "link") blank $ mconcat
      [ "rel" =: "icon"
      , "type" =: "image/png"
      , "sizes" =: size
      , "href" =: href
      ]
