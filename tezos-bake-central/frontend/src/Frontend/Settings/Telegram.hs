{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Settings.Telegram
  ( viewCfg
  , editCfg
  ) where

import Data.Function (on)
import Data.Map (Map)
import Data.Map.Monoidal (getMonoidalMap)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (validatedInput)
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (watchViewSelector)
import Safe (headMay)

import Common.Api
import Common.App (BakeView (..), BakeViewSelector (..))
import Common.Schema
import Common.Vassal (getRangeView', viewRangeAll)
import ExtraPrelude
import Frontend.Common

-- We take a dynamic `Maybe TelegramConfig` parameter rather than watching to
-- get `Maybe (Maybe TelegramConfig)`, so the caller can handle the
-- uninitialized case.

viewCfg
  :: MonadAppWidget js t m
  => Dynamic t TelegramConfig
  -> m (Event t ())
viewCfg cfg = do
  (reopener, _) <- elClass "p" "edit-link" $ do
    el' "a" $ text "Reconfigure Telegram"

  recipients <- watchTelegramRecipients
  elClass "table" "settings-table" $ do
    el "tr" $ do
      el "th" $ text "Recipient"
      el "th" $ text "Bot Name"
    void $ listWithKey recipients $ \_ recipient -> do
      el "tr" $ do
        el "td" $ dynText $ fmap telegramRecipientFullName recipient
        el "td" $ dynText $ fmap (view $ telegramConfig_botName . _Just) cfg

  return $ domEvent Click reopener

editCfg
  :: forall m t js
  .  MonadAppWidget js t m
  => Dynamic t (Maybe TelegramConfig)
  -> m (Event t ())
editCfg cfg = switchHold never <=< workflowView $ Workflow $ do
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
            text "No conversations found. Make sure you've started a conversation with your bot before trying again."

          let submit = filterRight $ tag (current botApiKey) $ gate (not <$> current isLoading) submitClick
          (isLoading, gotResponse) <- formIsLoading
            (\old new -> on (<) (^? _Just.telegramConfig_updated) old new && isJust (new ^? validated))
            cfg
            (void submit)
          submitState <- holdUniqDyn $ zipDynWith
            (\loading key -> if loading then Nothing else Just $ either (const Disabled) (const Enabled) key)
            isLoading botApiKey

        divClass "buttons" $
          uiDynSubmit submitState $ text "Connect Telegram"

        pure (submit, submitResult)

    _ <- requestingIdentity $ public . PublicRequest_AddTelegramConfig <$> submit_

    pure (never, Workflow . successPage <$> fmapMaybe id submitResult_)

  where
    heading = el "h3"

    successPage recipient = do
      heading $ text "Bot Connection Successful!"
      el "p" $ do
        text "We’ve sent a test message and will be sending notifications to "
        el "strong" $ text $ telegramRecipientFullName recipient
        text " from your bot."
      done <- divClass "buttons" $ uiButton "primary close" "Close"
      pure (done, never)

telegramRecipientFullName
  :: TelegramRecipient
  -> Text
telegramRecipientFullName recipient = _telegramRecipient_firstName recipient <> maybe "" (" " <>) (_telegramRecipient_lastName recipient)

settingsForm
  :: MonadAppWidget js t m
  => Dynamic t (Maybe TelegramConfig)
  -> m (Dynamic t (Either Text Text))
settingsForm cfg = holdUniqDyn =<< do
  botApiKey <- holdUniqDyn $ (^? _Just . telegramConfig_botApiKey) <$> cfg
  botApiKeyEvent <- updatedWithInit botApiKey

  let st t = el "strong" $ text t
  el "ol" $ do
    el "li" $ do
      text "Send " *> st "‘/newbot’" *> text " to the Telegram BotFather bot and follow the instructions to create a bot that Kiln will use to send notifications. If you've already made a bot, skip to the next step."
      el "p" $
        elAttr "a" ("href"=:"https://telegram.me/BotFather" <> "target"=:"_blank") $ do
          text "Start BotFather conversation " *> icon "icon-pop-out"

    v <- el "li" $ do
      text "After creating your bot, copy the " *> st "HTTP API Token" *> text " from the BotFather and paste it here:"
      divClass "field"
        $ validatedInput Validator.validateText
        $ def
          & Txt.setPlaceholder "e.g. 435389513:ABCDefGhij6K5l1m_NoPqRstUVWxyZ8AbCD"
          & Txt.setFluid
          & Txt.setChangeEvent (fromMaybe "" <$> botApiKeyEvent)

    el "li" $ text "Click the link to your bot that the BotFather gives you and send " *> st "‘/start’" *> text " to your bot."

    el "li"
      $ text "To receive direct messages from the bot, send any random message to it." *> el "br" blank
      *> text "To receive messages to a group, add the bot to a group. If you had already added the bot to a group, remove the bot from the group and add it again." *> el "br" blank
      *> text "This allows us to look up the bot’s recent conversation ID and use it to send messages to the correct place."

    el "li"
      $ text "You’re done! Click " *> st "‘Connect Telegram'" *> text " to finish linking Kiln to your bot!"

    return v
watchTelegramRecipients :: MonadAppWidget js t m => m (Dynamic t (Map (Id TelegramRecipient) TelegramRecipient))
watchTelegramRecipients =
  (fmap . fmap) (getMonoidalMap . fmapMaybe getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }
