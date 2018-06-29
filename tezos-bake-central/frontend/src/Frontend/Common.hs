{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Common where

import Control.Lens ((%~))
import Control.Monad.Reader (MonadReader, asks)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import qualified Reflex.Dom.TextField as Txt
import qualified Text.URI as Uri

import Common (tshow)
import Common.PublicKeyHash (PublicKeyHash, toPublicKeyHashText)
import Common.TaggedHash (BlockHash, toBase58Text)
import Common.Tez (Tez (..))
import Common.URI (appendPaths, mkRootUri)


newtype Cfg = Cfg { _cfg_blockExplorerUrl :: Maybe Uri.URI }


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
validateUri = Validator.Validator mkRootUri setUrlType
  where
    setUrlType cfg = cfg { Txt._textField_type = Txt.TextInputType "url" }


blockExplorerLink :: (MonadReader Cfg m, DomBuilder t m) => Text -> m a -> m a
blockExplorerLink path f = do
  urlCfg <- asks _cfg_blockExplorerUrl
  case urlCfg of
    Nothing -> f
    Just url -> elAttr "a" ("href"=:maybe "" Uri.render (url `appendPaths` [path]) <> "target"=:"_blank") f

blockHashLink :: (MonadReader Cfg m, DomBuilder t m) => BlockHash -> m ()
blockHashLink blockHash = blockHashLinkAs blockHash (text $ T.take 14 $ toBase58Text blockHash)

blockHashLinkAs :: (MonadReader Cfg m, DomBuilder t m) => BlockHash -> m a -> m a
blockHashLinkAs blockHash = blockExplorerLink (toBase58Text blockHash)

publicKeyHashLink :: (MonadReader Cfg m, DomBuilder t m) => PublicKeyHash -> m ()
publicKeyHashLink pkh = blockExplorerLink hash (text hash)
  where hash = toPublicKeyHashText pkh
