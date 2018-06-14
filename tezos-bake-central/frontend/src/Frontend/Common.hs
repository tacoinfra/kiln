{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Common where

import Control.Lens ((%~))
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core


buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button" <> "class" =: "ui button" <> "data-tooltip" =: t) $ do
    text label

-- | Simple form with a submit button that is disabled when the form is invalid.
simpleForm :: (DomBuilder t m, PostBuild t m) => Text -> Maybe Text -> m (Dynamic t (Maybe a)) -> m (Event t a)
simpleForm submitLabel submitTooltip form = do
  (formResult, submit) <- formWithSubmit $ do
    formResult <- form
    elDynAttr "button"
      (ffor formResult $ \r ->
        "type" =: "submit" <>
        "class" =: ("ui small button" <> if isJust r then "" else " disabled") <>
        maybe mempty ("data-tooltip" =:) submitTooltip
      )
      (text submitLabel)
    return formResult
  return $ fmapMaybe id $ tag (current formResult) submit

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
