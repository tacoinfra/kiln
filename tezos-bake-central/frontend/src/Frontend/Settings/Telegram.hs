{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Settings.Telegram where

import Data.Function (on)
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap, getMonoidalMap)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, watchViewSelector)
import Safe (headMay)

import Common.Api
import Common.App (Bake, BakeView (..), BakeViewSelector (..))
import Common.Schema hiding (Event)
import Common.Vassal (getMaybeView, getRangeView', viewJust, viewRangeAll)
import ExtraPrelude
import Frontend.Common (Enabled (..), cancelableModal, formIsLoading, formWithSubmit, icon, uiButton,
                        uiDynSubmit, updatedWithInit)
import Frontend.Modal.Class (HasModal (ModalM, tellModal))


inlineSettings :: forall m t. (MonadRhyoliteFrontendWidget Bake t m, MonadRhyoliteFrontendWidget Bake t (ModalM m), HasModal t m) => m ()
inlineSettings = do
  cfg <- watchTelegramConfig
  hasValidated <- holdUniqDyn $ preview (_Just . telegramConfig_validated . _Just) <$> cfg
  openTelegramOptions <- (switchHold never =<<) $ dyn $ ffor hasValidated $ \case
    Nothing -> uiButton "primary" "Connect Telegram"
    Just False -> uiButton "primary" "Connect Telegram"
    Just True -> do
      recipients <- watchTelegramRecipients
      (reopener, _) <- elClass "p" "edit-link" $ do
        el' "a" $ text "Reconfigure Telegram"
      elClass "table" "telegram-recipients" $ do
        el "tr" $ do
          elClass "th" "telegram-recipient" $ text "Recipient"
          elClass "th" "telegram-bot" $ text "Bot Name"
        void $ listWithKey recipients $ \_ recipient -> do
          el "tr" $ do
            elClass "td" "telegram-recipient" $ dynText $ fmap _telegramRecipient_fullName recipient
            elClass "td" "telegram-bot" $ dynText $ fmap (view $ _Just . telegramConfig_botName . _Just) cfg
      return $ domEvent Click reopener

  tellModal $ (openTelegramOptions $>) $ cancelableModal $ \close -> do
    finish <- settings
    pure $ leftmost [finish, close]

_telegramRecipient_fullName recipient = _telegramRecipient_firstName recipient <> maybe "" (" " <>) (_telegramRecipient_lastName recipient)

settings :: forall m t. MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
settings = switchHold never <=< workflowView $ Workflow $ do
  cfg <- watchTelegramConfig
  recipients <- watchTelegramRecipients
  let
    validated = _Just . telegramConfig_validated . _Just

    validatedRecipient :: Dynamic t (Maybe TelegramRecipient) = zipDynWith
      (\c recips -> if c ^? validated == Just True
        then headMay $ toList recips
        else Nothing
      )
      cfg recipients

  divClass "telegram-setup" $ do
    heading $ text "Setup Telegram Notifications"

    rec
      ((submit_, submitResult_), submitClick) <- formWithSubmit $ do
        botApiKey <- settingsForm cfg

        rec
          let submitResult = tagPromptlyDyn validatedRecipient gotResponse
          widgetHold_ blank $ ffor (isJust <$> submitResult) $ \isValid -> if isValid then blank else elClass "p" "error" $ do
            icon "red icon-warning-circle"
            text " No conversations found. Make sure your bot token is correct and you've recently sent a message to your bot before trying again."

          let submit = filterRight $ tag (current botApiKey) $ gate (not <$> current isLoading) submitClick
          (isLoading, gotResponse) <- formIsLoading
            (\old new -> on (<) (^? _Just.telegramConfig_updated) old new && isJust (new ^? validated))
            cfg
            (void submit)
          submitState <- holdUniqDyn $ zipDynWith
            (\loading key -> if loading then Nothing else Just $ either (const Disabled) (const Enabled) key)
            isLoading botApiKey

        horizontallyCentered $ do
          uiDynSubmit submitState $ text "Connect Telegram"

        pure (submit, submitResult)

    _ <- requestingIdentity $ public . PublicRequest_AddTelegramConfig <$> submit_

    pure (never, Workflow . successPage <$> fmapMaybe id submitResult_)

  where
    heading = el "h3"
    horizontallyCentered = elAttr "div" ("style"=:"text-align:center")

    successPage recipient = do
      heading $ text "Bot Connection Successful!"
      el "p" $ do
        text "We’ve sent a test message and will be sending notifications to "
        el "strong" $ text $ _telegramRecipient_fullName recipient
        text " from your bot."
      done <- horizontallyCentered $ uiButton "primary" "Close"
      pure (done, never)

settingsForm
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Maybe TelegramConfig)
  -> m (Dynamic t (Either Text Text))
settingsForm cfg = holdUniqDyn =<< do
  botApiKey <- holdUniqDyn $ (^? _Just . telegramConfig_botApiKey) <$> cfg
  botApiKeyEvent <- updatedWithInit botApiKey

  el "ol" $ do
    el "li" $ do
      text "Send \"/newbot\" to the Telegram BotFather bot and create a bot that will be used to send you notifications regarding your Kiln systems. If you've already made a bot, skip to the next step."
      el "p" $
        elAttr "a" ("href"=:"https://telegram.me/BotFather" <> "target"=:"_blank") $ do
          text "Start BotFather conversation " *> icon "icon-pop-out"

    el "li" $ text "Send \"/start\" to your new bot, or if you've already started your bot, just send any random message. This allows us to look up your recent conversation ID and use it to send you alerts."
    el "li" $ do
      text "After creating your bot enter your bot token here:"
      divClass "field"
        $ validatedInput Validator.validateText
        $ def
          & Txt.setPlaceholder "eg 435389513:ABCDefGhij6K5l1m_NoPqRstUVWxyZ8AbCD"
          & Txt.setFluid
          & Txt.setChangeEvent (fromMaybe "" <$> botApiKeyEvent)

watchTelegramRecipients :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Map (Id TelegramRecipient) TelegramRecipient))
watchTelegramRecipients =
  (fmap . fmap) (getMonoidalMap . fmapMaybe getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }

watchTelegramConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe TelegramConfig))
watchTelegramConfig =
  (fmap . fmap) (getMaybeView . _bakeView_telegramConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramConfig = viewJust 1 }
