{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.Common where

import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, asks)
import Data.Fixed (divMod')
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (TimeZone, UTCTime)
import qualified Data.Time as Time
import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public, ApiRequest)
import Rhyolite.Frontend.App (MonadRhyoliteWidget)
import qualified Text.URI as Uri
import Text.URI.QQ (uri)

import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM as DOM
import qualified "ghcjs-dom" GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM

import Tezos.Types
  (BlockHash, NamedChain(..), PublicKeyHash, Tez(..), blockHashToBase58Text, toPublicKeyHashText)

import Common (humanizeTimestamp,humanizeTimestampWithoutTZ)
import Common.Api (PublicRequest, PrivateRequest)
import Common.Alerts (
    ErrorDescription(..),
    standardTimeFormat,
  )
import Common.App
import Common.Config (FrontendConfig, HasFrontendConfig (frontendConfig), frontendConfig_chain, parseBakerAddr)
import Common.URI (appendPaths, mkRootUri)
import ExtraPrelude

type MonadAppWidget js t m = (MonadRhyoliteFrontendWidget js (BakeViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m)

type MonadRhyoliteFrontendWidget js q r t m =
     ( MonadRhyoliteWidget q r t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadIO m
     , MonadIO (Performable m)
    , Prerender js t m
     )

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
urlLink = hrefLink . Uri.render

hrefLink :: DomBuilder t m => Text -> m a -> m a
hrefLink href = elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener")

blackhrefLink :: DomBuilder t m => Text -> m a -> m a
blackhrefLink href = elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener" <> "style" =: "color:  #000000")

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

fancyTez :: DomBuilder t m => Tez -> m ()
fancyTez t = let (w, p, tz) = tezPadded t in elClass "span" "fancy-tez" $ do
  text $ w <> p
  elClass "span" "tez" $ text tz

-- | Clickable copy-to-clipboard icon
copyButton
  :: (SemUi.UI js t m, MonadJSM (Performable m))
  => Behavior t Text -- ^ Text to copy to clipboard
  -> m ()
copyButton content = mdo
  let conf = ffor state $ ("class" =:) . \case
        Nothing -> "blue icon-copy link icon"
        Just True -> "green icon-check icon"
        Just False -> "red icon-x icon"
  copy <- fmap fst $ elDynAttr' "i" conf blank
  result <- copyToClipboard $ tag content $ domEvent Click copy
  delayed <- delay 1 result
  state <- holdDyn Nothing $ leftmost [Just <$> result, Nothing <$ delayed]
  pure ()

-- | Copy the given text to the clipboard
copyToClipboard
  :: (MonadJSM (Performable m), PerformEvent t m)
  => Event t Text
  -- ^ Text to copy to clipboard. Event must come directly from user
  -- interaction (e.g. domEvent Click), or the copy will not take place.
  -> m (Event t Bool)
  -- ^ Did the copy take place successfully?
copyToClipboard copy = performEvent $ ffor copy $ \t -> do
  doc <- DOM.currentDocumentUnchecked
  ta <- DOM.uncheckedCastTo TextArea.HTMLTextAreaElement <$> Document.createElement doc ("textarea" :: Text)
  TextArea.setValue ta t
  body <- Document.getBodyUnchecked doc
  _ <- Node.appendChild body ta
  HTMLElement.focus ta
  TextArea.select ta
  success <- Document.execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  _ <- Node.removeChild body ta
  pure success

data TooltipPos
  = TooltipPos_TopLeft
  | TooltipPos_TopCenter
  | TooltipPos_TopRight
  | TooltipPos_CenterRight
  | TooltipPos_CenterLeft
  | TooltipPos_BottomLeft
  | TooltipPos_BottomCenter
  | TooltipPos_BottomRight

data TooltipConfig = TooltipConfig
  { _tooltipConfig_pos :: TooltipPos
  }
makeLenses ''TooltipConfig

defaultTooltipConfig :: TooltipConfig
defaultTooltipConfig = TooltipConfig
  TooltipPos_TopCenter

defaultWrapper
  :: DomBuilder t m
  => forall b. m b
  -> m (Element EventResult (DomBuilderSpace m) t, b)
defaultWrapper =
  elAttr' "span" ("style" =: "position:relative")

tooltipped
  :: SemUi.UI js t m
  => MonadIO (Performable m)
  => TooltipPos -> m () -> m a -> m a
tooltipped pos = tooltippedWrapper defaultWrapper pos

tooltippedWrapper
  :: SemUi.UI js t m
  => MonadIO (Performable m)
  => (forall b. m b -> m (Element EventResult (DomBuilderSpace m) t, b))
  -- ^ Wrapper (used to determine mouse events)
  -> TooltipPos -> m () -> m a -> m a
tooltippedWrapper wrapper pos = tooltippedWithConfig c wrapper
  where
    c = defaultTooltipConfig & tooltipConfig_pos .~ pos

tooltippedWithConfig
  :: SemUi.UI js t m
  => MonadIO (Performable m)
  => TooltipConfig
  -> (forall b. m b -> m (Element EventResult (DomBuilderSpace m) t, b))
  -> m ()
  -> m a
  -> m a
tooltippedWithConfig TooltipConfig { .. } wrapper tip w = mdo
  let (cls, x, y, transform) = case _tooltipConfig_pos of
        TooltipPos_TopLeft -> ("top left", "left: 0", "top: 0", "(0, -110%)")
        TooltipPos_TopCenter -> ("top center", "left: 50%", "top: 0", "(-50%, -110%)")
        TooltipPos_TopRight -> ("top right", "right: 0", "top: 0", "(0, -110%)")
        TooltipPos_CenterLeft -> ("center left", "left: -10px", "top: 50%", "(-100%, -50%)")
        TooltipPos_CenterRight -> ("center right", "left: 100%", "top: 50%", "(0, -50%)")
        TooltipPos_BottomLeft -> ("bottom left", "left: 0", "top:100%", "(0,0)")
        TooltipPos_BottomCenter -> ("bottom center", "left:50%", "top:100%", "(-50%, 0)")
        TooltipPos_BottomRight -> ("bottom right", "right: 0", "top:100%", "(0,0)")

  (wEl, a) <- wrapper $ do
    a' <- w
    let mouseenter = True <$ domEvent Mouseenter wEl
        mouseleave = False <$ domEvent Mouseleave wEl
    hovered' <- throttle 0.75 $ leftmost [mouseenter, mouseleave]
    hovered <- fmap updated . holdUniqDyn <=< holdDyn False $ leftmost [mouseenter, hovered']
    open <- transitionEvent (\wasHovering isHovering -> guard $ not wasHovering && isHovering) False hovered
    close <- transitionEvent (\wasHovering isHovering -> guard $ wasHovering && not isHovering) False hovered
    let changeEvent = leftmost [ SemUi.In <$ open, SemUi.Out <$ close ]
    _ <- SemUi.ui "span" (def
      & SemUi.classes .~ ("ui popup" <> cls)
      & SemUi.style .~ fromString (intercalate "; "
                           [ "width: max-content"
                           , "max-width: unset"
                           , x
                           , y
                           , "transform: translate" <> transform
                           ])
      & SemUi.action .~ Just def
        { SemUi._action_initialDirection = SemUi.Out
        , SemUi._action_transition = ffor changeEvent $ \transition -> SemUi.Transition SemUi.Drop (Just transition) (def { SemUi._transitionConfig_duration = 0 }) -- oddly, the above "transform: translate" is visibly re-applied during a non-instant transition in the 'disconnected' tooltip
        , SemUi._action_transitionStateClasses = SemUi.forceVisible
        }) tip
    pure a'
  pure a

whenJustDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> (a -> m ()) -> m ()
whenJustDyn d f = dyn_ . ffor d $ \case
  Nothing -> blank
  Just x -> f x

localTimestamp :: (DomBuilder t m, MonadReader r m, HasTimeZone r) => Time.UTCTime -> m ()
localTimestamp t = do
  tz <- asks (^. timeZone)
  text $ T.pack $ Time.formatTime Time.defaultTimeLocale standardTimeFormat $ Time.utcToZonedTime tz t

localHumanizedTimestamp
  ::
    ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m
    , MonadReader r m, HasTimeZone r, HasTimer t r, Prerender js t m
    )
  => Dynamic t (Maybe Text)
  -> Dynamic t Time.UTCTime
  -> m ()
localHumanizedTimestamp titleDyn tsDyn = do
  tz <- asks (^. timeZone)
  currentTime <- asks (^. timer)
  tooltipped TooltipPos_BottomLeft
    (do
      whenJustDyn titleDyn $ \title -> el "strong" (text title) *> el "br" blank
      dyn_ $ fmap localTimestamp tsDyn
    ) $
    dynText <=< holdUniqDyn $ ffor2 currentTime tsDyn $ humanizeTimestamp tz

-- | Like 'localHumanizedTimestamp' for tooltips without titles. Uses CSS
-- tooltips since they are more lightweight
-- The actual string displayed is given by the first argument 'humanize'.
localHumanizedTimestampBasicGen
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadReader r m, HasTimeZone r, HasTimer t r)
  => (TimeZone -> UTCTime -> UTCTime -> Text)
  -> Dynamic t Time.UTCTime
  -> m ()
localHumanizedTimestampBasicGen humanize tsDyn = do
  tz <- asks (^. timeZone)
  currentTime <- asks (^. timer)
  let attrs = ffor tsDyn $ \ts -> M.fromList
        [ ("data-position", "bottom left")
        , ("data-tooltip", T.pack $ Time.formatTime Time.defaultTimeLocale standardTimeFormat $ Time.utcToZonedTime tz ts)
        ]
  elDynAttr "span" attrs $ dynText <=< holdUniqDyn $ ffor2 currentTime tsDyn $ humanize tz

-- | Like 'localHumanizedTimestamp' for tooltips without titles. Uses CSS
-- tooltips since they are more lightweight
localHumanizedTimestampBasic
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadReader r m, HasTimeZone r, HasTimer t r)
  => Dynamic t Time.UTCTime
  -> m ()
localHumanizedTimestampBasic tsDyn = localHumanizedTimestampBasicGen humanizeTimestamp tsDyn

-- | Like 'localHumanizedTimestampBasic' but don't show the timezone
localHumanizedTimestampBasicWithoutTZ
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadReader r m, HasTimeZone r, HasTimer t r)
  => Dynamic t Time.UTCTime
  -> m ()
localHumanizedTimestampBasicWithoutTZ tsDyn = localHumanizedTimestampBasicGen humanizeTimestampWithoutTZ tsDyn


uiButton :: DomBuilder t m => Text -> Text -> m (Event t ())
uiButton classes = uiButtonM classes . text

uiButtonM :: DomBuilder t m => Text -> m () -> m (Event t ())
uiButtonM classes label = fmap (domEvent Click . fst) $
  elAttr' "button" ("type" =: "button" <> "class" =: ("ui " <> classes <> " button")) label

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

buttonIconWithInfoCls :: (DomBuilder t m) => Text -> Text -> Text -> m (Event t ())
buttonIconWithInfoCls i classes label =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button" <> "class" =: ("ui button " <> classes)) $ do
    icon i
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
tooltipPos = tooltipPos' ""

tooltipPos' :: (DomBuilder t m) => Text -> Text -> Text -> m a -> m a
tooltipPos' cls p t = elAttr "div" ("class" =: cls <> "data-tooltip" =: t <> "data-position" =: p)

-- | Builds a form element and captures the submit event.
formWithSubmit :: (DomBuilder t m, PostBuild t m) => m a -> m (a, Event t ())
formWithSubmit f = do
  (el_, r) <- elDynAttrWithModifyEvent' preventDefault Submit "form" (pure $ "class"=:"ui form") f
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


-- | Like 'elDynWithModifyConfig'' but only configures the 'EventFlags'.
elDynAttrWithModifyEvent'
  :: forall en t m a. (DomBuilder t m, PostBuild t m)
  => EventFlags
  -> EventName en              -- ^ Event on the element to configure with 'preventDefault'
  -> Text                      -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a                       -- ^ Child of element
  -> m (Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrWithModifyEvent' f ev = elDynAttrWithModifyConfig'
  (\elCfg -> elCfg & elementConfig_eventSpec %~
    addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) ev (const f))

validateUri :: Validator.Validator t m Uri.URI
validateUri =  Validator.Validator mkRootUri setUrlType
  where
    setUrlType cfg = cfg { Txt._textField_type = Txt.TextInputType "url" }

validateBakerAddr :: Validator.Validator t m PublicKeyHash
validateBakerAddr = Validator.Validator
  -- TODO human readable error message
  parseBakerAddr
  id

tzStatsBlockUri :: NamedChain -> Text -> Maybe Uri.URI
tzStatsBlockUri chain path = (`appendPaths` [path]) $ case chain of
  NamedChain_Babylonnet -> [uri|http://babylonnet.tzstats.com/|]
  NamedChain_Carthagenet -> [uri|http://carthagenet.tzstats.com/|]
  NamedChain_Zeronet -> [uri|http://zeronet.tzstats.com/|]
  NamedChain_Mainnet -> [uri|http://tzstats.com/|]
  NamedChain_Delphinet -> [uri|http://delphinet.tzstats.com/|]
  NamedChain_Edonet -> [uri|https://edo.tzstats.com|] -- this will produce a Nothing value,
  NamedChain_Edo2net -> [uri|https://edo.tzstats.com|] -- this will produce a Nothing value,
  NamedChain_Florencenet -> [uri|https://florence.tzstats.com|] -- this will produce a Nothing value,
  NamedChain_Granadanet -> [uri|https://granada.tzstats.com/|]
  NamedChain_Hangzhounet ->[uri|https://hangzhou.tzstats.com/|]
  NamedChain_Ithacanet -> [uri|https://ithaca.tzstats.com/|] -- this will produce a Nothing value
  NamedChain_Jakartanet -> [uri|https://jakarta.tzstats.com/|] -- this will produce a Nothing value
  NamedChain_Ghostnet -> [uri|https://ghost.tzstats.com/|]
  NamedChain_Kathmandunet -> [uri|https://kathmandu.tzstats.com/|] -- this will produce a Nothing value
  NamedChain_Limanet -> [uri|https://lima.tzstats.com/|] -- this will produce a Nothing value

blockExplorerLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
blockExplorerLink dPath f = do
  chain <- asks (^. frontendConfig . frontendConfig_chain)
  case chain of
    Right _chainId -> f
    Left namedChain ->
      elDynAttr "a"
        (ffor dPath $ \path ->
          "href"=:maybe "" Uri.render (tzStatsBlockUri namedChain path) <> "target"=:"_blank")
        f

blockHashLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t BlockHash -> m ()
blockHashLink blockHash = blockHashLinkAs blockHash (dynText $ T.take 14 . blockHashToBase58Text <$> blockHash)

blockHashLinkAs :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => Dynamic t BlockHash -> m a -> m a
blockHashLinkAs blockHash = blockExplorerLink (blockHashToBase58Text <$> blockHash)

publicKeyHashLink :: (MonadReader r m, HasFrontendConfig r, DomBuilder t m, PostBuild t m) => PublicKeyHash -> m ()
publicKeyHashLink pkh = blockExplorerLink (pure hash) (text hash)
  where hash = toPublicKeyHashText pkh

iconClass :: Text -> Text
iconClass i = "ui " <> i <> " icon"

icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" (iconClass i) blank

iconDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
iconDyn iDyn = elDynAttr "i" (ffor iDyn $ \i -> "class" =: iconClass i) blank

kilnLogo :: DomBuilder t m => m ()
kilnLogo = blackhrefLink "https://tezos-kiln.org/" $ elAttr "img" ("src" =: static @"images/logo.svg" <> "class" =: "app-logo") blank

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

cancelableModal :: (DomBuilder t m, PostBuild t m, MonadFix m) => (Event t () -> m (Event t ())) -> Event t () -> m (Event t ())
cancelableModal f = cancelableModalWithClasses (fmap (pure [],) . f)

cancelableModalWithClasses
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => (Event t () -> m (Dynamic t [Text], Event t ())) -> Event t () -> m (Event t ())
cancelableModalWithClasses f close = mdo
  (classes, e) <- elDynAttr "div" (ffor classes $ \cs -> "class"=:T.unwords ("modal-box":cs)) $ do
    (closeEl, _) <- elAttr' "div" ("class"=:"modal-close") $ elClass "i" "icon-x fitted icon" blank
    divClass "content" (f $ leftmost [domEvent Click closeEl, close])
  pure e

reminderModal :: MonadAppWidget js t m
                  => Text
                  -> Text
                  -> Text
                  -> (Event t () -> Event t (PublicRequest ()))
                  -> Event t ()
                  -> m (Event t ())
reminderModal title msg = confirmationModal False title [msg]

warningModal :: (MonadAppWidget js t m)
             => Text
             -> [Text]
             -> Text
             -> (Event t () -> Event t (PublicRequest ()))
             -> Event t ()
             -> m (Event t ())
warningModal = confirmationModal True

confirmationModal :: (MonadAppWidget js t m)
                  => Bool
                  -> Text
                  -> [Text]
                  -> Text
                  -> (Event t () -> Event t (PublicRequest ()))
                  -> Event t ()
                  -> m (Event t ())
confirmationModal isDangerous title msgs btn mkReq = cancelableModalWithClasses $ \close -> do
  divClass "ui header" $ do
    when isDangerous $ icon "icon-warning big red"
    text title
  for_ msgs $ el "p" . text
  confirm <- divClass "buttons" $ uiButton "primary" btn
  response <- requestingIdentity $ public <$> mkReq confirm
  pure (pure ["confirmation"], leftmost [response, close])

data MenuState = MenuState_Closed | MenuState_Opened | MenuState_PendingClose
  deriving (Eq, Show, Ord)

manageMenu
  :: forall menu m t.
    ( PerformEvent t m, TriggerEvent t m, MonadHold t m, MonadFix m, MonadIO (Performable m)
    , HasDomEvent t menu 'MouseleaveTag, HasDomEvent t menu 'MouseenterTag
    )
  => Event t ()
  -> menu
  -> m (Event t SemUi.Direction)
manageMenu click menuEl = mdo
  afterPendingClose <- delay 1 $ ffilter (== MenuState_PendingClose) $ updated menuState
  menuState <- holdUniqDyn <=< foldDyn ($) MenuState_Closed $ leftmost
    [ click $> \case
        MenuState_Closed -> MenuState_PendingClose
        MenuState_Opened -> MenuState_Closed
        MenuState_PendingClose -> MenuState_Closed
    , domEvent Mouseleave menuEl $> \case
        MenuState_Closed -> MenuState_Closed
        MenuState_Opened -> MenuState_PendingClose
        MenuState_PendingClose -> MenuState_PendingClose
    , domEvent Mouseenter menuEl $> \case
        MenuState_Closed -> MenuState_Closed
        MenuState_Opened -> MenuState_Opened
        MenuState_PendingClose -> MenuState_Opened
    , afterPendingClose $> \case
        MenuState_Closed -> MenuState_Closed
        MenuState_Opened -> MenuState_Opened
        MenuState_PendingClose -> MenuState_Closed
    ]
  open <- transitionEvent
    (\a b -> if a == MenuState_Closed && b /= MenuState_Closed then Just () else Nothing)
    MenuState_Closed
    (updated menuState)
  close <- transitionEvent
    (\a b -> if a /= MenuState_Closed && b == MenuState_Closed then Just () else Nothing)
    MenuState_Closed
    (updated menuState)

  pure $ leftmost [ SemUi.In <$ open, SemUi.Out <$ close ]

uriField :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
         => Text -> Text -> m (Dynamic t (Either Text Uri.URI))
uriField lbl ph = validatedInput validateUri $ def
  & Txt.setPlaceholder (if ph == "" then "" else "e.g. " <> ph)
  & Txt.setFluid
  & Txt.addLabel (el "label" $ text lbl)

pkhField :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
         => Text -> Text -> m (Dynamic t (Either Text PublicKeyHash))
pkhField lbl ph = validatedInput validateBakerAddr $ def
  & Txt.setPlaceholder ("e.g. " <> ph)
  & Txt.setFluid
  & Txt.addLabel (el "label" $ text lbl)

aliasField :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
           => Text -> m (Dynamic t (Either Text (Maybe Text)))
aliasField ph = validatedInput (Validator.optional Validator.validateText) $ def
  & Txt.setPlaceholder ("e.g. " <> ph)
  & Txt.setFluid
  & Txt.addLabel (el "label" $ text "Alias")

textField :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
           => m (Dynamic t (Either Text (Maybe Text)))
textField = validatedInput (Validator.optional Validator.validateText) $ def
  & Txt.setFluid

minConnectionsField :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Num a, Ord a, Read a, Show a)
                    => m (Dynamic t (Either Text (Maybe a)))
minConnectionsField = validatedInput (Validator.optional $ Validator.validateNumeric mempty (Just 0, Nothing) Nothing) $ def
  & Txt.setPlaceholder ("e.g. " <> "5")
  & Txt.setFluid
  & Txt.addLabel (el "label" $ do
                     text "Minimum Peer Connections"
                     divClass "explanation" $ text "Kiln will fire an alert if the node is connected to fewer than this many peers.")

zipFields :: (Applicative m, Reflex t)
          => m (Dynamic t (Either Text a))
          -> m (Dynamic t (Either Text b))
          -> m (Dynamic t (Either Text (a, b)))
zipFields = zipFieldsWith (,)

zipFieldsWith :: (Applicative m, Reflex t)
              => (a -> b -> c)
              -> m (Dynamic t (Either Text a))
              -> m (Dynamic t (Either Text b))
              -> m (Dynamic t (Either Text c))
zipFieldsWith = liftA2 . liftA2 . liftA2

formWithReset
  :: forall js a m t. (MonadAppWidget js t m)
  => Text -- ^ Form label
  -> Text -- ^ Submit button tooltip
  -> m () -- ^ Feedback after submit
  -> Event t () -- ^ Reset the form
  -> m (Dynamic t (Either Text a)) -- ^ Fields
  -> m (Event t a)
formWithReset lbl ttp feedback reset fields = divClass "ui form fields" $ do
  (val, submitEvt) <- formWithSubmit $ do
    val <- fmap join $ widgetHold fields $ fields <$ reset
    feedback
    _ <- submitButtonWithInfoCls "fluid primary" lbl ttp
    pure val
  return $ filterRight $ current val <@ submitEvt

nbsp :: Text
nbsp = "\x00A0"

errorLabel :: (DomBuilder t m, Traversable f) => Text -> f Text -> m ()
errorLabel primary secondary = el "div" $ do
  el "label" $ text primary
  el "wbr" blank
  for_ secondary $ \x -> do
    elClass "label" "secondary-label monospaced-text" $ do
      text x

nodeLabel :: DomBuilder t m => NodeSummary -> m ()
nodeLabel = uncurry errorLabel . nodeSummaryIdentification

bakerSummaryLabel :: DomBuilder t m => PublicKeyHash -> BakerSummary -> m ()
bakerSummaryLabel = curry $ uncurry errorLabel . bakerSummaryIdentification

ensureHealthyNodes :: DomBuilder t m => m ()
ensureHealthyNodes = do
  text "Add a node from the left panel or make sure any nodes you’ve already added are"
  icon "circle healthy-node small green"
  text "healthy."

htmlErrorDescription :: DomBuilder t m => ErrorDescription -> m ()
htmlErrorDescription = \case
  ErrorDescription_Plain t -> text t
  ErrorDescription_Emphasis t -> el "strong" $ text t
  ErrorDescription_Concat t t' -> ((*>) `on` htmlErrorDescription) t t'

makeLenses ''FrontendContext

instance HasFrontendConfig (FrontendContext t) where
  frontendConfig = frontendContext_config

instance HasTimeZone (FrontendContext t) where
  timeZone = frontendContext_timeZone

instance HasTimer t (FrontendContext t) where
  timer = frontendContext_oneSecondTimer

-- UI element with left pointing arrow
backButton :: DomBuilder t m => m (Event t ())
backButton = do
  (e, _) <- elClass' "div" "back-button" $ do
    icon "icon-angle-left blue"
    el "span" $ text "Back"
  pure $ domEvent Click e

isRadioItemSelected
  :: ( Reflex t
     , MonadHold t m
     )
  => [Event t ()]
  -> Event t ()
  -> Bool
  -> m (Dynamic t Bool)
isRadioItemSelected radioItems option selectedByDefault =
  holdDyn selectedByDefault $ leftmost $ curOption : otherOptions
  where
    curOption    = True <$ option
    otherOptions = map (\opt -> False <$ opt) radioItems

shortenPkh :: PublicKeyHash -> Text
shortenPkh pkh =
  let pkhText = toPublicKeyHashText pkh
  in T.take 12 pkhText <> "..." <> T.takeEnd 4 pkhText

pkhTooltip :: (DomBuilder t m) => PublicKeyHash -> m ()
pkhTooltip = divClass "tooltip-description" . elClass "p" "monospaced-text" . text . toPublicKeyHashText

monospacedPkhText :: (DomBuilder t m) => PublicKeyHash -> m ()
monospacedPkhText = elClass "span" "monospaced-text" . text . toPublicKeyHashText
