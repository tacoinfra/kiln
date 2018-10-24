{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Settings.Telegram where

import Control.Lens.TH (makePrisms)
import Data.Map.Monoidal (MonoidalMap)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (validatedInput)
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, watchViewSelector)
import Safe (headMay)

import ExtraPrelude
import Common.Api
import Common.App (Bake, BakeView (..), BakeViewSelector (..))
import Common.Schema hiding (Event)
import Common.Vassal (getMaybeView, getRangeView', viewJust, viewRangeAll)
import Frontend.Common (formWithSubmit, uiButton, uiDynSubmit, updatedWithInit, Enabled(..))

data PageState v = PageState_Unsubmitted | PageState_Submitted | PageState_Success v
  deriving (Functor, Eq, Ord, Show, Generic, Typeable)
makePrisms ''PageState

setSuccess :: v -> PageState v -> PageState v
setSuccess v PageState_Submitted = PageState_Success v
setSuccess v PageState_Success{} = PageState_Success v
setSuccess _ s = s

settings :: forall m t. MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
settings = switchHold never <=< workflowView $ Workflow $ do
  cfg <- watchTelegramConfig
  recipients <- watchTelegramRecipients

  divClass "telegram-setup" $ do
    heading $ text "Setup Telegram Notifications"
    rec
      (botApiKey, submitClick) <- formWithSubmit $ do
        botApiKey_ <- settingsForm cfg

        widgetHold_ blank $ ffor validated $ \isValid -> if isValid then blank else elClass "p" "error" $ do
          elClass "i" "icon-warning-circle red icon" blank
          text " No conversations found. Make sure your bot token is correct and you've recently sent a message to your bot before trying again."

        -- TODO: Abstract this somewhere.
        rec
          submitState <- holdUniqDyn <=< holdDyn (Just Disabled) $ leftmost
            [ Nothing <$ submit -- Loading
            , Just . either (const Disabled) (const Enabled) <$> leftmost -- Revalidate when input changes or validation result comes back.
                [ gate (isJust <$> current submitState) (updated botApiKey) -- Allow input changes only when not in loading state.
                , tag (current botApiKey) validated -- This will exit loading state when validation comes in.
                ]
            ]
        horizontallyCentered $ do
          uiDynSubmit submitState $ text "Connect Telegram"

        pure botApiKey_

      let
        submit = filterRight (tag (current botApiKey) submitClick)
        validated = fmapMaybe (^? _Just . telegramConfig_validated . _Just) $ updated cfg

    _ <- requestingIdentity $ public . PublicRequest_AddTelegramConfig <$> submit

    let
      validatedRecipient :: Dynamic t (Maybe TelegramRecipient) = zipDynWith
        (\c recips -> if c ^? _Just . telegramConfig_validated . _Just == Just True
          then headMay $ fmapMaybe id $ toList recips
          else Nothing
        )
        cfg recipients

    state :: Dynamic t (PageState TelegramRecipient) <- foldDyn ($) PageState_Unsubmitted $ leftmost
      [ const PageState_Submitted <$ submit
      , setSuccess <$> fmapMaybe id (updated validatedRecipient)
      ]

    pure (never, Workflow . successPage <$> fmapMaybe (^? _PageState_Success) (updated state))

  where
    heading = el "h3"
    horizontallyCentered = elAttr "div" ("style"=:"text-align:center")

    successPage recipient = do
      heading $ text "Bot Connection Successful!"
      el "p" $ do
        text "We’ve sent a test message and will be sending notifications to "
        el "strong" $ text $
          _telegramRecipient_firstName recipient <> maybe "" (" " <>) (_telegramRecipient_lastName recipient)
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
          text "Start BotFather conversation " *> elClass "i" "icon-pop-out icon" blank

    el "li" $ text "Send \"/start\" to your new bot, or if you've already started your bot, just send any random message. This allows us to look up your recent conversation ID and use it to send you alerts."
    el "li" $ do
      text "After creating your bot enter your bot token here:"
      divClass "field"
        $ validatedInput Validator.validateText
        $ def
          & Txt.setPlaceholder "eg 435389513:ABCDefGhij6K5l1m_NoPqRstUVWxyZ8AbCD"
          & Txt.setFluid
          & Txt.setChangeEvent (fromMaybe "" <$> botApiKeyEvent)

watchTelegramRecipients :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id TelegramRecipient) (Maybe TelegramRecipient)))
watchTelegramRecipients =
  (fmap . fmap) (fmap getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }

watchTelegramConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe TelegramConfig))
watchTelegramConfig =
  (fmap . fmap) (getMaybeView . _bakeView_telegramConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramConfig = viewJust 1 }
