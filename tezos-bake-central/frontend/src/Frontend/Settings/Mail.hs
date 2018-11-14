{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Settings.Mail where

import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first, second)
import Data.Functor.Infix
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Form.Checks as Check
import GHCJS.DOM.Types (MonadJSM)
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import Rhyolite.Schema (Email)

import Common.Api
import Common.App
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Common

abstractPassword :: Text
abstractPassword = "••••••••••••"

mailServerForm
  :: ( MonadRhyoliteFrontendWidget Bake t m
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => (MailServerView, [Email]) -> m (Event t ((MailServerView, Maybe Text), [Email]))
mailServerForm (srv0, emails0) = do
  (form, save) <- formWithSubmit $ do
    srvform <- serverFields
    mailform <- mailNotificationOptions
    let form = (liftA2 . liftA2) (,) srvform mailform
    elDynAttr "button"
      (ffor (isRight <$> form) $ \s -> "type"=:"submit"
        <> "class"=:("ui tiny primary submit button" <> if s then "" else " disabled")
      ) $ text "Save Email Settings"
    return form

  -- TODO think about passwords forms and optional fields
  let form' = (fmap . first . second)
        (\t -> guard (t /= abstractPassword) *> Just t)
        (filterRight $ tag (current form) save)

  pure form'

  where
    mailNotificationOptions = do
      divClass "notification-settings-description" $ text "Enter email address to receive email notifications below."

      let
        emailWidget email = elClass "span" "email-buttons" $ do
          (remove, _) <- elClass' "a" "remove" $ icon "icon-x"
          (send, _) <- elClass' "a" "send-test" $ text "Send Test Email"
          void $ requestingIdentity $ public . PublicRequest_SendTestEmail <$> (current email <@ domEvent Click send)
          pure $ domEvent Click remove

      listInput "Add email address" (isRight . Check.email) emailWidget emails0

    serverFields = withFormFieldsErr (srv0, "") $ do
      divClass "three fields" $ do
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "required four wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host"
            & Txt.setInitial (_mailServerView_hostName srv0)
            & Txt.setPlaceholder "eg 127.0.0.1"

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "required three wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port"
            & Txt.setInitial (tshow $ _mailServerView_portNumber srv0)
            & Txt.setPlaceholder "eg 465"

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "required three wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol srv0)
              never
              $ SemUi.TaggedStatic
              $ SmtpProtocol_Plain=:text "Plain"
              <> SmtpProtocol_Ssl=:text "SSL"
              <> SmtpProtocol_Starttls=:text "STARTTLS"

      divClass "two fields" $ do
        tellFieldErr (_1 . mailServerView_userName) <=< formItem' "four wide"
          $ validatedInput (Validator.optionalWith "" id Validator.validateText)
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName srv0)

        tellFieldErr _2 <=< formItem' "three wide"
          $ validatedInput (Validator.optionalWith "" id validatePassword)
          $ Txt.setInitial abstractPassword
          $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text

-- | Control that allows the user to build a list of items.
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> (Dynamic t Text -> m (Event t ())) -- ^ Widget builder for each item in the list
          -> [Text] -- ^ Initial items
          -> m (Dynamic t (Either Text [Text])) -- ^ Items in list
listInput ph validate itemWidget items0 = mdo
  let insert is = if any T.null is
                  then is
                  else Map.insert (succ . maybe 0 fst $ Map.lookupMax is) "" is

      validation t = if T.null t
                     then Right t
                     else submission t

      submission = Check.satisfies validate "Invalid email address"

  items' <- holdUniqDyn <=< holdDyn (Map.fromList $ zip [0::Int ..] items0) $ leftmost
    [ insert <$> updated items
    , attachWith (flip Map.delete) (current items) $ fmap getFirst delete
    ]

  (items, delete) <- runEventWriterT $ fmap joinDynThroughMap $ listWithKey items' $ \k v -> divClass "ui fields" $ do
    divClass "ui inline field" $ mdo
      postBuild <- getPostBuild
      txt <- fmap value $ inputElement $ def
        & initialAttributes .~ ("placeholder" =: ph)
        & inputElementConfig_setValue .~ (current v <@ postBuild)
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updated validationAttrs

      let
        validationAttrs = ffor (validation <$> txt) $ mapKeysToAttributeName . ("class" =:) . \case
          Left _ -> Just "invalid"
          Right _ -> Nothing

      dyn_ $ ffor (submission <$> txt) $ \case
        Left _ -> blank
        Right _ -> do
          del <- lift $ itemWidget txt
          tellEvent $ First k <$ del

      pure txt

  pure $ fmap sequence $ fmap submission . ffilter (not . T.null) . Map.elems <$> items
