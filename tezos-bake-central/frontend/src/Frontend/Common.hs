{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Common where

import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.ByteString.Base16 as BS16
import Data.Fixed (divMod')
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (TimeZone, UTCTime)
import qualified Data.Time as Time
import Data.Version (Version, showVersion)
import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import qualified Text.URI as Uri

import Tezos.Base58Check (HashBase58Error(..))
import Tezos.NodeRPC.Sources (tzScanUri)
import Tezos.ShortByteString (fromShort)
import Tezos.PublicKeyHash (tryReadPublicKeyHashText)
import Tezos.Types (BlockHash, Fitness, PublicKeyHash, Tez (..), toBase58Text, toPublicKeyHashText, unFitness)

import Common (humanizeTimestamp)
import Common.Api (PublicRequest)
import Common.App (Bake, BakerSummary(..), NodeSummary,
                   bakerSummaryIdentification, nodeSummaryIdentification)
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
urlLink = hrefLink . Uri.render

hrefLink :: DomBuilder t m => Text -> m a -> m a
hrefLink href = elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener")

tez :: Tez -> Text
tez t = let (w, p, tz) = tez' t
         in w <> p <> tz

tez' :: Tez -> (Text, Text, Text)
tez' (Tez n) = (T.pack wholes', parts', "ꜩ")
  where (wholes :: Integer, parts) = n `divMod'` 1
        wholes' = reverse $ f $ reverse $ show wholes
        parts' = T.dropWhileEnd (== '.')
                 $ T.dropAround (== '0')
                 $ tshow parts
        f = \case
          (a0 : a1 : a2 : as) | as /= [] -> a0 : a1 : a2 : ',' : f as
          as -> as

standardTimeFormat :: String
standardTimeFormat = "%A, %b %-d, %Y @ %-l:%M%P %Z"

localTimestamp :: (DomBuilder t m, MonadReader r m, HasTimeZone r) => Time.UTCTime -> m ()
localTimestamp t = do
  tz <- asks (^. timeZone)
  text $ T.pack $ Time.formatTime Time.defaultTimeLocale standardTimeFormat $ Time.utcToZonedTime tz t

localHumanizedTimestamp
  ::
    ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m
    , MonadReader r m, HasTimeZone r, HasTimer t r
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
localHumanizedTimestampBasic
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadReader r m, HasTimeZone r, HasTimer t r)
  => Dynamic t Time.UTCTime
  -> m ()
localHumanizedTimestampBasic tsDyn = do
  tz <- asks (^. timeZone)
  currentTime <- asks (^. timer)
  let attrs = ffor tsDyn $ \ts -> M.fromList
        [ ("data-position", "bottom left")
        , ("data-tooltip", T.pack $ Time.formatTime Time.defaultTimeLocale standardTimeFormat $ Time.utcToZonedTime tz ts)
        ]
  elDynAttr "span" attrs $ dynText <=< holdUniqDyn $ ffor2 currentTime tsDyn $ humanizeTimestamp tz

data TooltipPos
  = TooltipPos_TopLeft
  | TooltipPos_TopCenter
  | TooltipPos_TopRight
  | TooltipPos_CenterRight
  | TooltipPos_CenterLeft
  | TooltipPos_BottomLeft
  | TooltipPos_BottomCenter
  | TooltipPos_BottomRight

tooltipped
  :: forall a m t.
    ( DomBuilder t m
    , PostBuild t m
    , MonadHold t m
    , MonadFix m
    , PerformEvent t m
    , MonadIO (Performable m)
    , TriggerEvent t m
    )
  => TooltipPos -> m () -> m a -> m a
tooltipped pos tip w = mdo
  let (cls, x, y, transform) = case pos of
        TooltipPos_TopLeft -> ("top left", "left: 0", "top: 0", "(0, -110%)")
        TooltipPos_TopCenter -> ("top center", "left: 50%", "top: 0", "(-50%, -110%)")
        TooltipPos_TopRight -> ("top right", "right: 0", "top: 0", "(0, -110%)")
        TooltipPos_CenterLeft -> ("center left", "left: -10px", "top: 50%", "(-100%, -50%)")
        TooltipPos_CenterRight -> ("center right", "left: 100%", "top: 50%", "(0, -50%)")
        TooltipPos_BottomLeft -> ("bottom left", "left: 0", "top:100%", "(0,0)")
        TooltipPos_BottomCenter -> ("bottom center", "left:50%", "top:100%", "(-50%, 0)")
        TooltipPos_BottomRight -> ("bottom right", "right: 0", "top:100%", "(0,0)")

  (wEl, a) <- elAttr' "span" ("style" =: "position:relative") $ do
    a' <- w
    let
      hovered = leftmost [ True <$ domEvent Mouseenter wEl, False <$ domEvent Mouseleave wEl ]
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
buttonIconWithInfoCls i classes label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button" <> "class" =: ("ui button " <> classes) <> "data-tooltip" =: t) $ do
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
  checkBakerAddr
  id

checkBakerAddr :: Text -> Either Text PublicKeyHash
checkBakerAddr v = do
  when (not $ T.take 3 v `elem` okPrefixes) $ do
    Left $ (if T.take 3 v == "KT1" then "\"KT1\" addresses cannot bake. Address" else "Baker address") <> " must begin with " <> conjList ", " " or " (NE.map tshow okPrefixes) <> "."
  for_ (T.find (isNothing . flip T.find "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" . (==)) v) $ \ch ->
    Left $ "The character " <> tshow ch <> " is not allowed in a baker address."
  when (T.length v /= 36) $ Left $ "Baker address is too " <> (if T.length v < 36 then "short" else "long") <> " (must be 36 characters)."
  flip first (tryReadPublicKeyHashText v) $ \case
    HashBase58Error_InvalidPrefix _ _ -> "This address is outside the valid range for " <> T.take 3 v <> " addresses."
    HashBase58Error_BadChecksum _ _ _ -> "This address failed the integrity check. Please check that it has been copied correctly."
    e -> "An unknown error happened, please report this as a bug: " <> tshow e
  where
    okPrefixes :: NE.NonEmpty Text
    okPrefixes = "tz1" :| ["tz2", "tz3"]

conjList :: Text -> Text -> NE.NonEmpty Text -> Text
conjList comma conj = go
  where
    go xs = case NE.uncons xs of
      (x, Nothing) -> x
      (x, Just (y :| [])) -> x <> conj <> y
      (x, Just xs') -> x <> comma <> go xs'

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

iconClass :: Text -> Text
iconClass i = "ui " <> i <> " icon"

icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" (iconClass i) blank

iconDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
iconDyn iDyn = elDynAttr "i" (ffor iDyn $ \i -> "class" =: iconClass i) blank

kilnLogo :: DomBuilder t m => m ()
kilnLogo = elAttr "img" ("src" =: static @"images/logo.svg" <> "class" =: "app-logo") blank

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
cancelableModal = cancelableModalWithClasses []

cancelableModalWithClasses :: DomBuilder t m => [Text] -> (Event t () -> m (Event t ())) -> Event t () -> m (Event t ())
cancelableModalWithClasses classes f close = elAttr "div" ("class"=:T.unwords ("modal-box":classes)) $ do
  (closeEl, _) <- elAttr' "div" ("class"=:"modal-close") $ elClass "i" "icon-x fitted icon" blank
  divClass "content" (f $ leftmost [domEvent Click closeEl, close])

confirmationModal :: MonadRhyoliteFrontendWidget app t m
                  => Text
                  -> Text
                  -> Text
                  -> (Event t () -> Event t (PublicRequest app ()))
                  -> Event t ()
                  -> m (Event t ())
confirmationModal title msg btn mkReq = cancelableModal $ \close -> do
  el "h3" $ text title
  el "p" $ text msg
  confirm <- divClass "buttons" $ uiButton "primary" btn
  response <- requestingIdentity $ public <$> mkReq confirm
  pure $ leftmost [response, close]

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
  & Txt.setPlaceholder ("e.g. " <> ph)
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
zipFieldsWith = (liftA2 . liftA2 . liftA2)

formWithReset
  :: forall a m t. (MonadRhyoliteFrontendWidget Bake t m)
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
  for_ secondary $ \x -> do
    elClass "label" "secondary-label" $ do
      text nbsp
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

makeLenses ''FrontendContext

instance HasFrontendConfig (FrontendContext t) where
  frontendConfig = frontendContext_config

instance HasTimeZone (FrontendContext t) where
  timeZone = frontendContext_timeZone

instance HasTimer t (FrontendContext t) where
  timer = frontendContext_oneSecondTimer
