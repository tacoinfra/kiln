{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Common where

import Control.Lens ((%~))
import Data.List (find)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.URI as Uri
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import qualified Reflex.Dom.TextField as Txt

import Common (tshow)
import Common.Tez (Tez (..))


tez :: Tez -> Text
tez (Tez n) = T.dropWhileEnd (=='.') (T.dropWhileEnd (== '0') (tshow n)) <> "ꜩ"


uiButton :: DomBuilder t m => Text -> Text -> m (Event t ())
uiButton classes label = fmap (domEvent Click . fst) $
  elAttr' "button" ("type" =: "button" <> "class" =: ("ui " <> classes <> " button")) $ text label

buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button" <> "class" =: "ui button" <> "data-tooltip" =: t) $ do
    text label


tooltip :: (DomBuilder t m) => Text -> m a -> m a
tooltip t = elAttr "div" ("data-tooltip" =: t)

tooltipPos :: (DomBuilder t m) => Text -> Text -> m a -> m a
tooltipPos p t = elAttr "div" ("data-tooltip" =: t <> "data-position" =: p)


-- | Builds a form element and captures the submit event.
formWithSubmit :: (DomBuilder t m, PostBuild t m) => m a -> m (a, Event t ())
formWithSubmit f = do
  (el_, r) <- elDynAttrWithPreventDefaultEvent' Submit "form" (pure $ "class"=:"ui form") f
  pure (r, domEvent Submit el_)

-- | Like 'elDynAttr'' but allows you to modify the element configuration.
elDynAttrWithModifyConfig'
  :: forall t m a. (DomBuilder t m, PostBuild t m)
  => (ElementConfig EventResult t (DomBuilderSpace m) -> ElementConfig EventResult t (DomBuilderSpace m))
  -> Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithModifyConfig' f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element elementTag (f cfg) child
  notReadyUntil =<< getPostBuild
  pure result


-- | Like 'elDynAttr'' but configures "prevent default" on the given event.
elDynAttrWithPreventDefaultEvent'
  :: forall en t m a. (DomBuilder t m, PostBuild t m)
  => EventName en              -- ^ Event on the element to configure with 'preventDefault'
  -> Text                      -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a                       -- ^ Child of element
  -> m (Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrWithPreventDefaultEvent' ev = elDynAttrWithModifyConfig'
  (\elCfg -> elCfg & elementConfig_eventSpec %~
    addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) ev (const preventDefault))


validateUri :: Validator.Validator t m Uri.URI
validateUri = Validator.Validator checkUri setUrlType
  where
    checkUri txt = case Uri.parseURI $ T.unpack txt of
      Nothing -> Left "Please enter a valid URI"
      Just uri -> maybe (Right uri) (Left . snd) $ find fst
        [ (T.toLower (T.pack $ Uri.uriScheme uri) `notElem` ["http:", "https:"], "URL scheme must be http or https")
        , (T.null $ maybe "" (T.strip . T.pack . Uri.uriRegName) (Uri.uriAuthority uri), "URL must have a host name or IP address")
        , (not $ null $ Uri.uriQuery uri, "URL must not have a query")
        , (not $ null $ Uri.uriFragment uri, "URL must not have a fragment")
        ]

    setUrlType cfg = cfg { Txt._textField_type = Txt.TextInputType "url" }
