{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Settings where

import Control.Monad (guard)
import Data.Function (on)
import Data.Functor.Infix
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Version (showVersion)
import GHCJS.DOM.Types (MonadJSM)
import Prelude hiding (log)
import Reflex.Dom.Core
import qualified Reflex.Dom.SemanticUI as SemUi
import Rhyolite.Api (public)
import Text.Read (readMaybe)

import Common.Api
import Common.App
import Common.Distribution
import Common.Config (HasFrontendConfig (frontendConfig), frontendConfig_appVersion,
                      frontendConfig_checkForUpgrade)
import Common.Schema
import ExtraPrelude
import Frontend.Common
import Frontend.Modal.Class (HasModal (ModalM, tellModal))
import qualified Frontend.Settings.Telegram as Telegram
import qualified Frontend.Settings.Mail as Mail
import Frontend.Watch

type EditFun t cfg m = Dynamic t (Maybe cfg) -> m (Event t ()) -- state change

data NotificationCfg m t = forall cfg. NotificationCfg
  { _notificationCfg_name :: Text
  , _notificationCfg_description :: Text
  , _notificationCfg_iconName :: Text
  , _notificationCfg_view :: Dynamic t cfg -> m (Event t ())
  , -- | The edit setting widget. Either is used to control whether editing
    -- should switch what is displayed or open a modal.
    _notificationCfg_edit :: Either
      (EditFun t cfg (ModalM m)) -- Left for modal
      (EditFun t cfg m)          -- Right for non Modal
  , _notificationCfg_method :: AlertNotificationMethod
  , -- Outer Maybe means not loaded
    _notificationCfg_watchCfg :: m (Dynamic t (Maybe (Maybe cfg)))
  , _notificationCfg_getEnabled :: cfg -> Bool
  }

-- | If editing is instead a modal 'SettingsRoute_Edit' is not used.
--
-- TODO incorporate into consistent routing framework somehow. Keep in mind that
-- there are two settings sections, mail and route, so the page route is
-- '(SettingsRoute, SettingsRoute)'.
data SettingsRoute t cfg
  = SettingsRoute_Button
  | SettingsRoute_Disabled
  | SettingsRoute_View (Dynamic t cfg)
  | SettingsRoute_Edit

settingsTab
  :: forall r t m js.
    ( MonadAppWidget js t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , HasModal t m, MonadAppWidget js t (ModalM m)
    )
  => m ()
settingsTab = do
  divClass "version-section" $ do
    currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
    divClass "soft-heading" $ text $ "Kiln Version " <> T.pack (showVersion currentVersion)

    enableUpgradeCheck <- asks (^. frontendConfig . frontendConfig_checkForUpgrade)
    when enableUpgradeCheck upgradeOptions

  SemUi.divider def

  divClass "notifications-section" $ do
    SemUi.header (def & SemUi.headerConfig_size SemUi.|?~ SemUi.H3) $ do
      text "Notification Channels"
      SemUi.subHeader $ text "Configure channels to send notifications out from Kiln."

    divClass "notification-settings-section" $ sequence_ $ intersperse (SemUi.divider def) $ map notificationSection
      [ NotificationCfg
        { _notificationCfg_name = "Email"
        , _notificationCfg_description = "Use your own email server to send alerts."
        , _notificationCfg_iconName = "letter"
        , _notificationCfg_view = Mail.viewCfg
        , _notificationCfg_edit = Right Mail.editCfg
        , _notificationCfg_method = AlertNotificationMethod_Email
        , _notificationCfg_watchCfg = watchMailServer
        , _notificationCfg_getEnabled = _mailServerView_enabled

        }
      , NotificationCfg
        { _notificationCfg_name = "Telegram"
        , _notificationCfg_description = "Use a Telegram Bot to send alerts."
        , _notificationCfg_iconName = "telegram"
        , _notificationCfg_view = Telegram.viewCfg
        , _notificationCfg_edit = Left Telegram.editCfg
        , _notificationCfg_method = AlertNotificationMethod_Telegram
        , _notificationCfg_watchCfg = watchTelegramConfig
        , _notificationCfg_getEnabled = _telegramConfig_enabled
        }
      ]

  SemUi.divider def

  divClass "notifications-section" $ do
    SemUi.header (def & SemUi.headerConfig_size SemUi.|?~ SemUi.H3) $ do
      text "Notification Settings"
      SemUi.subHeader $ text "Configure settings for various Kiln notifications."

    divClass "notification-settings-section" $ sequence_ $ intersperse (SemUi.divider def) $ map notificationSettings [ RightKind_Baking, RightKind_Endorsing ]

  where
    notificationSettings :: RightKind -> m ()
    notificationSettings rk = do
      mLimit <- watchRightNotificationLimit rk
      initialLimit <- sample $ current mLimit
      let textKind = case rk of
            RightKind_Baking -> "bake"
            RightKind_Endorsing -> "endorsement"
      divClass "ui tiny header" $ text $ "Missed " <> T.toTitle textKind
      (every, ()) <- fakeRadioItem (isNothing <$> mLimit) $ text $ "Notify for every missed " <> textKind
      rec
        (after, rnlDyn) <- fakeRadioItem (isJust <$> mLimit) $ mdo
          let input f = do
                rec result <- fmap (fmap (readMaybe . T.unpack) . value) $ inputElement $ def
                      & initialAttributes .~ "type" =: "number" <> "min" =: "0"
                      & inputElementConfig_initialValue .~ maybe "1" (tshow . f) initialLimit
                      & inputElementConfig_setValue .~ leftmost
                        [ fforMaybe (updated mLimit) (fmap $ tshow . f)
                        -- Set value to "1" if there's nothing there and the user just selected this option
                        , attachWithMaybe (\m () -> maybe (Just "1") (const Nothing) m) (current result) after
                        ]
                pure result
          text "Notify when "
          amount <- input _rightNotificationLimit_amount
          text $ " or more " <> textKind <> "s are missed within "
          within <- input _rightNotificationLimit_withinMinutes
          text " minutes"
          pure $ ffor2 amount within $ liftA2 $ \a w -> RightNotificationLimit
            { _rightNotificationLimit_amount = a
            , _rightNotificationLimit_withinMinutes = w
            }
      choice <- throttle 1 $ leftmost
        [ Nothing <$ every
        , Just <$> attachWithMaybe (\rnl () -> rnl) (current rnlDyn) after
        , updated rnlDyn
        ]
      choice' <- holdUniqDyn <=< holdDyn Nothing $ Just <$> choice
      _ <- requestingIdentity $ public . PublicRequest_SetRightNotificationSettings rk <$> fmapMaybe id (updated choice')
      pure ()

    notificationSection :: NotificationCfg m t -> m ()
    notificationSection (NotificationCfg name descr iconName viewCfg editCfg method watchCfg (getEnabled :: cfg -> Bool)) =
      divClass "notifications-subsection" $ do
        dmdmCfg <- maybeDyn =<< watchCfg
        dyn_ $ ffor dmdmCfg $ \case
          Nothing -> divClass "ui active centered inline text loader"
            $ text $ name <> " notification settings loading."
          Just dmCfg' -> do
            -- If the settings are invalid, we turn the `Just cfg` to
            -- nothing. This conflates invalid saved data with no saved
            -- data.
            let
              headerIconText = do
                icon ("icon-" <> iconName)
                text name
            dmdCfg <- maybeDyn dmCfg'
            SemUi.header
              (def
                & SemUi.headerConfig_size SemUi.|?~ SemUi.H5
                )
              $ dyn_ $ ffor (getEnabled <$$$> dmdCfg) $ \case
                -- If nothing is set, return nothing
                Nothing -> headerIconText
                -- If something is set, the enable toggle should appear and
                -- return the value of that config.
                Just (dEnabled :: Dynamic t Bool) -> do
                  pb <- getPostBuild
                  let setVal = leftmost [updated dEnabled, tag (current dEnabled) pb]
                  toggleSwitch <- SemUi.checkbox
                    headerIconText
                    (def
                      & SemUi.checkboxConfig_type SemUi.|?~ SemUi.Toggle
                      & SemUi.checkboxConfig_setValue . SemUi.initial .~ True
                      & SemUi.checkboxConfig_setValue . SemUi.event .~ Just setVal
                      )
                  -- Set enabled state based on toggle.
                  statuses <- requestingIdentity $ fmap (public . PublicRequest_SetAlertNotificationMethodEnabled method) $
                    updated $ toggleSwitch ^. SemUi.checkbox_value
                  void $ runWithReplace (pure ()) $ ffor statuses $ \case
                    True -> pure ()
                    False -> fail $ show $ "\
                      \Can't enable unconfigured " <> iconName <> " notifications. \
                      \It is a bug that the user even had a toggle to click in this case."
            rec
              let fromEnabled :: Dynamic t (SettingsRoute t cfg) = dmdCfg >>= \case
                    Nothing -> pure SettingsRoute_Button
                    Just dCfg -> ffor (getEnabled <$> dCfg) $ \case
                      True -> SettingsRoute_View dCfg
                      False -> SettingsRoute_Disabled
              (eEdit :: Event t Bool) <- (=<<) (switchHold never) $ dyn $ ffor route $ \case
                SettingsRoute_Button -> do
                  divClass "notification-settings-description" $ text descr
                  True <$$ uiButton "fluid" ("Connect " <> name)
                SettingsRoute_View dcfg -> do
                  divClass "notification-settings-description" $ text descr
                  True <$$ viewCfg dcfg
                SettingsRoute_Edit -> do
                  divClass "notification-settings-description" $ text descr
                  editCfg' <- case editCfg of
                    Left _ -> fail "don't switch routes for modal version"
                    Right ec -> pure ec
                  False <$$ editCfg' dmCfg'
                SettingsRoute_Disabled -> do
                  divClass "notification-settings-description" $ text $ name <> " notifications are turned off"
                  pure never
              (dEdit :: Dynamic t Bool) <- holdDyn False eEdit
              -- Implement the switch case. There is no modal so the button
              -- instead sets the edit route.
              let route = if isRight editCfg
                    then ffor2 fromEnabled dEdit $ curry $ \case
                      (SettingsRoute_Disabled, _) -> SettingsRoute_Disabled
                      (normal, False) -> normal
                      (_, True) -> SettingsRoute_Edit
                    else fromEnabled
              -- Implement the modal case. The button triggers the modal and
              -- does not change the route.
              case editCfg of
                Right _ -> pure ()
                Left editCfg' -> tellModal $ (fmapMaybe guard eEdit $>) $
                  cancelableModal $ \close -> do
                    finish <- editCfg' dmCfg'
                    pure $ leftmost [finish, close]
            pure ()

    upgradeOptions = do
      currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
      upstreamVersion <- watchUpstreamVersion

      isLoading <- elClass "p" "check-for-updates" $ mdo
        (e, _) <- el' "a" $ text "Check for updates"
        dyn_ $ ffor isLoading $ \case
          True -> divClass "ui tiny active blue inline loader" blank *> text " Checking for updates..."
          False -> pure ()
        let submit = gate (not <$> current isLoading) $ domEvent Click e
        (isLoading, _gotResponse) <- formIsLoading ((<) `on` (^? _Just . upstreamVersion_updated)) upstreamVersion submit
        _ <- requestingIdentity $ public PublicRequest_CheckForUpgrade <$ submit
        pure isLoading

      dyn_ $ ffor2 upstreamVersion isLoading $ \v' loading -> case loading of
        True -> pure ()
        False -> case v' of
          Just UpstreamVersion { _upstreamVersion_error = Just _e } -> text "Unable to reach update server."
          Just UpstreamVersion { _upstreamVersion_version = Just v, _upstreamVersion_updated = updatedTime } ->
            if v > currentVersion
            then do
              elAttr "div" ("class" =: "ui tiny header" <> "style" =: "margin-bottom: 1rem") $ do
                icon "upgrade-icon icon-arrow-up"
                text ("Kiln " <> T.pack (showVersion v) <> " is available!")
              let uri = "https://gitlab.com/tezos-kiln/kiln/-/releases"
              el "p" $ do
                text "Release notes: "
                hrefLink uri $ text uri
              case distributionMethod of
                Distribution_FromSource -> blank
                Distribution_Docker -> el "p" $ text $ "You are running Kiln via Docker. To update, quit Kiln and run the ‘docker run’ command using the tag ‘" <> T.pack (showVersion v) <> "’."
                Distribution_LinuxPackage -> el "p" $ text "To update, install the latest version using whichever package manager you are using to manage Kiln."
            else
              text "Up to date as of " *> localHumanizedTimestamp (pure Nothing) (pure updatedTime)
          _ -> blank

fakeRadioItem :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m (Event t (), a)
fakeRadioItem checked ma = do
  (e, a) <- elDynAttr' "div" (ffor checked $ \c -> "class" =: ("fake-radio-item" <> if c then " checked" else "")) ma
  pure (domEvent Click e, a)

(<$$) :: Functor f => Functor g => b -> f (g a) -> f (g b)
(<$$) b fga = fmap (fmap (const b)) fga
