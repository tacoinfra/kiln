{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Common where

import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.ByteString.Base16 as BS16
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (TimeZone, UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Format.Human as HumanTime
import Data.Version (Version, showVersion)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import qualified Reflex.Dom.TextField as Txt
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (tzScanUri)
import Tezos.ShortByteString (fromShort)
import Tezos.Types (BlockHash, Fitness, PublicKeyHash, Tez (..), toBase58Text, toPublicKeyHashText, unFitness)

import Common.Config (FrontendConfig, HasFrontendConfig (frontendConfig), changelogUrl, frontendConfig_chain,
                      frontendConfig_upgradeBranch)
import Common.URI (appendPaths, mkRootUri)
import ExtraPrelude

data FrontendContext t = FrontendContext
  { _frontendContext_config :: !FrontendConfig
  , _frontendContext_timeZone :: !TimeZone
  , _frontendContext_oneSecondTimer :: !(Dynamic t UTCTime)
  } deriving (Generic, Typeable)

class HasTimeZone r where
  timeZone :: Lens' r TimeZone

class HasTimer t r where
  timer :: Lens' r (Dynamic t UTCTime)

data Enabled = Disabled | Enabled
  deriving (Eq, Ord, Show, Read, Enum)

isDisabled :: Enabled -> Bool
isDisabled Disabled = True
isDisabled Enabled = False

isEnabled :: Enabled -> Bool
isEnabled Enabled = True
isEnabled Disabled = False

urlLink :: DomBuilder t m => Uri.URI -> m a -> m a
urlLink url = elAttr "a" ("href"=:Uri.render url <> "target"=:"_blank")

tez :: Tez -> Text
tez (Tez n) = T.dropWhileEnd (=='.') (T.dropWhileEnd (== '0') (tshow n)) <> "ꜩ"

localTimestamp :: (DomBuilder t m, MonadReader r m, HasTimeZone r, PostBuild t m) => Dynamic t Time.UTCTime -> m ()
localTimestamp t = do
  tz <- asks (^. timeZone)
  dynText $ T.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" .
    Time.utcToZonedTime tz <$> t

localHumanizedTimestamp :: (DomBuilder t m, PostBuild t m, MonadReader r m, HasTimeZone r, HasTimer t r) => Dynamic t Time.UTCTime -> m ()
localHumanizedTimestamp tDyn = do
  tz <- asks (^. timeZone)
  currentTime <- asks (^. timer)
  dynText $ ffor2 currentTime tDyn $ \c t ->
    T.pack $ HumanTime.humanReadableTimeI18N' HumanTime.defaultHumanTimeLocale { HumanTime.timeZone = tz } c t

whenJustDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> (a -> m ()) -> m ()
whenJustDyn d f = dyn_ . ffor d $ \case
  Nothing -> blank
  Just x -> f x


uiButton :: DomBuilder t m => Text -> Text -> m (Event t ())
uiButton classes label = fmap (domEvent Click . fst) $
  elAttr' "button" ("type" =: "button" <> "class" =: ("ui " <> classes <> " button")) $ text label

uiDynSubmit :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe Enabled) -> m () -> m ()
uiDynSubmit state = elDynAttr "button" (ffor state $ \s ->
  "type"=:"submit" <> "class"=:("ui " <> stateClass s <> " primary button"))
  where
    stateClass = \case
      Just Enabled -> ""
      Just Disabled -> "disabled"
      Nothing -> "loading"

uiDynButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m () -> m (Event t ())
uiDynButton classes label = fmap (domEvent Click . fst) $
  elDynAttr' "button" (ffor classes $ \c -> "type"=:"button" <> "class"=:("ui " <> c <> " button")) label

modalOpeningButton :: (DomBuilder t m) => Text -> Text -> m (Event t ())
modalOpeningButton = buttonWithInfoCls ""

buttonIconWithInfoCls :: (DomBuilder t m) => Text -> Text -> Text -> Text -> m (Event t ())
buttonIconWithInfoCls icon classes label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button" <> "class" =: ("ui button " <> classes) <> "data-tooltip" =: t) $ do
  elClass "i" ("icon " <> icon) blank
  text label

buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo = buttonWithInfoCls ""

buttonWithInfoCls :: (DomBuilder t m) => Text -> Text -> Text -> m (Event t ())
buttonWithInfoCls = typedButtonWithInfoCls "button"

submitButtonWithInfoCls :: (DomBuilder t m) => Text -> Text -> Text -> m (Event t ())
submitButtonWithInfoCls = typedButtonWithInfoCls "submit"

typedButtonWithInfoCls :: (DomBuilder t m) => Text -> Text -> Text -> Text -> m (Event t ())
typedButtonWithInfoCls typ classes label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: typ <> "class" =: ("ui button " <> classes) <> "data-tooltip" =: t) $ do
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

blockExplorerLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
blockExplorerLink dPath f = do
  chain <- asks (^. frontendConfig . frontendConfig_chain)
  case chain of
    Right _chainId -> f
    Left namedChain ->
      elDynAttr "a" (ffor dPath $ \path -> "href"=:maybe "" Uri.render (tzScanUri namedChain `appendPaths` [path]) <> "target"=:"_blank") f

blockHashLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t BlockHash -> m ()
blockHashLink blockHash = blockHashLinkAs blockHash (dynText $ T.take 14 . toBase58Text <$> blockHash)

blockHashLinkAs :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t BlockHash -> m a -> m a
blockHashLinkAs blockHash = blockExplorerLink (toBase58Text <$> blockHash)

publicKeyHashLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => PublicKeyHash -> m ()
publicKeyHashLink pkh = blockExplorerLink (pure hash) (text hash)
  where hash = toPublicKeyHashText pkh

fitnessText :: Fitness -> Text
fitnessText = T.intercalate ":" . toList . fmap (T.decodeUtf8 . BS16.encode . fromShort) . unFitness

changelogLink :: (DomBuilder t m, MonadReader r m, HasFrontendConfig r) => Text -> Version -> m a -> m a
changelogLink cls version f = do
  asks (^. frontendConfig . frontendConfig_upgradeBranch) >>= \case
    Nothing -> f
    Just upgradeBranch -> elAttr "a"
      (  "class"=:cls
      <> "href"=:(changelogUrl upgradeBranch <> "#" <> versionAnchor)
      <> "target"=:"_blank") f
  where
    versionText = T.pack (showVersion version)
    versionAnchor = "anchor-" <> T.filter (/='.') versionText

-- | Terrible hack.
updatedWithInit :: PostBuild t m => Dynamic t a -> m (Event t a)
updatedWithInit d = do
  pb <- getPostBuild
  pure $ leftmost [updated d, tag (current d) pb]

-- | Lazier version of 'maybeDyn'. Very hacky.
maybeDynLazy
  :: (PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Maybe a) -> m (Dynamic t (Maybe (Dynamic t a)))
maybeDynLazy d = maybeDyn =<< holdDyn Nothing =<< updatedWithInit d

-- | Folds over events and triggers a new event when a given transition is detected.
transitionEvent
  :: forall a b m t. (Reflex t, MonadHold t m, MonadFix m)
  => (a -> a -> Maybe b) -- ^ Transition detection function.
                         -- First argument is the older value, second argument is the new value.
  -> a -- ^ Initial state for fold
  -> Event t a -- ^ Event to fold over
  -> m (Event t b)
transitionEvent f a0 e =
  fmap (fmapMaybe snd . updated) $
    foldDyn ($) (a0, Nothing) $ e <&> \newA (oldA, _) -> (newA, f oldA newA)

-- | Calculates the "loading" state of a form that is listening to a 'Dynamic' for its results.
formIsLoading
  :: forall state m t. (Reflex t, MonadHold t m, MonadFix m)
  => (state -> state -> Bool) -- ^ Function from old and new state (in that order) to a Bool indicating that the new state indicates a submission.
  -> Dynamic t state -- ^ Dynamic result
  -> Event t () -- ^ Submit event
  -> m (Dynamic t Bool, Event t ()) -- ^ 'Dynamic' for whether the form is loading, and an 'Event' for when the state changed after submit
formIsLoading comp state submitted = do
  isLoading <- (fmap.fmap) isJust $ foldDynMaybe ($) Nothing $ leftmost
    [ tagPromptlyDyn state submitted <&> \s _ -> Just (Just s)
    , updated state <&> \newState oldState ->
        if liftA2 comp oldState (Just newState) == Just True
          then Just Nothing
          else Nothing
    ]
  gotResult <- transitionEvent (\wasLoading nowLoadding -> if wasLoading && not nowLoadding then Just () else Nothing) False (updated isLoading)
  pure (isLoading, gotResult)

basicModal :: DomBuilder t m => m a -> m a
basicModal = elAttr "div" ("class"=:"modal-box") . divClass "content"

cancelableModal :: DomBuilder t m => (Event t () -> m (Event t ())) -> Event t () -> m (Event t ())
cancelableModal f close = elAttr "div" ("class"=:"modal-box") $ do
  (closeEl, _) <- elAttr' "div" ("class"=:"modal-close") $ elClass "i" "icon-x fitted icon" blank
  divClass "content" (f $ leftmost [domEvent Click closeEl, close])

makeLenses ''FrontendContext

instance HasFrontendConfig (FrontendContext t) where
  frontendConfig = frontendContext_config

instance HasTimeZone (FrontendContext t) where
  timeZone = frontendContext_timeZone

instance HasTimer t (FrontendContext t) where
  timer = frontendContext_oneSecondTimer
