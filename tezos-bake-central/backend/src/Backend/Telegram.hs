{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Telegram where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync)
import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug, logWarn)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Database.Groundhog.Postgresql (AutoKeyField (..), Postgresql, in_, limitTo, project, (&&.), (=.),
                                      (==.))
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import qualified Network.URI.Encode as UriEncode
import Rhyolite.Backend.DB (getTime, runDb)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv, runLoggingEnv)
import Safe (maximumMay, minimumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Backend.Common (nominalDiffTimeToMicroseconds, threadDelay', worker', workerWithDelay)
import Backend.Http (HasHttp, runHttpT)
import qualified Backend.Http as Http
import Backend.Schema
import Common (defaultTezosCompatJsonOptions, tshow)
import Common.Schema
import Common.URI as Uri (appendPaths, appendQueryParams)

data Env = Env
  { _env_beginWaitingForNewRecipient :: IO ()
  , _env_waitForNewRecipient :: IO ()
  } deriving (Typeable, Generic)

initState
  :: (IO () -> IO ())
  -> Http.Manager
  -> LoggingEnv
  -> Pool Postgresql
  -> IO Env
initState addFinalizer httpMgr logger db = do
  newRecipientChan <- newEmptyMVar
  let
    env = Env
      { _env_beginWaitingForNewRecipient = putMVar newRecipientChan ()
      , _env_waitForNewRecipient = takeMVar newRecipientChan
      }
  addFinalizer =<< telegramWorker httpMgr logger db (_env_waitForNewRecipient env)
  addFinalizer =<< emptyTelegramMessageQueue httpMgr logger db
  pure env

telegramApiBotUri :: Text -> Maybe URI
telegramApiBotUri botApiKey = appendPaths
  [Uri.uri|https://api.telegram.org|]
  [UriEncode.encodeTextWith (\c -> c == ':' || UriEncode.isAllowed c) $ "bot" <> botApiKey]

telegramApiGetMeUri :: Text -> Maybe URI
telegramApiGetMeUri botApiKey = telegramApiBotUri botApiKey >>= flip appendPaths ["getMe"]

data TelegramGetUpdates = TelegramGetUpdates
  { _telegramGetUpdates_botApiKey :: !Text
  , _telegramGetUpdates_offset :: !(Maybe Word64) -- ID of first message to show
  , _telegramGetUpdates_timeout :: !NominalDiffTime -- Long-polling timeout
  } deriving (Eq, Ord, Show, Typeable, Generic)

telegramApiGetUpdatesUri :: TelegramGetUpdates -> Maybe URI
telegramApiGetUpdatesUri cfg =
  telegramApiBotUri (_telegramGetUpdates_botApiKey cfg)
  >>= flip appendPaths ["getUpdates"]
  >>= flip appendQueryParams
    ([ ("offset", T.pack $ UriEncode.encode $ show offset) | Just offset <- [_telegramGetUpdates_offset cfg] ]
    <>
    [ ("timeout", T.pack $ UriEncode.encode $ show $ _telegramGetUpdates_timeout cfg)
    , ("allowed_updates", "messages")
    ])

data SendMessageRequest = SendMessageRequest
  { _sendMessageRequest_chatId :: !Word64
  , _sendMessageRequest_text :: !Text
  , _sendMessageRequest_parseMode :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Typeable, Generic)

telegramApiSendMessageUri :: Text -> Maybe URI
telegramApiSendMessageUri botApiKey =
  telegramApiBotUri botApiKey
  >>= flip appendPaths ["sendMessage"]

newtype UnixTimestamp = UnixTimestamp { unUnixTimestamp :: UTCTime }
  deriving (Eq, Ord, Show)
instance FromJSON UnixTimestamp where
  parseJSON = fmap (UnixTimestamp . posixSecondsToUTCTime) . Aeson.parseJSON
instance ToJSON UnixTimestamp where
  toJSON (UnixTimestamp t) = Aeson.toJSON $ utcTimeToPOSIXSeconds t
  toEncoding (UnixTimestamp t) = Aeson.toEncoding $ utcTimeToPOSIXSeconds t

data ApiResult a = ApiResult
  { _apiResult_ok :: !Bool
  , _apiResult_result :: !a
  } deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable, Traversable)

data BotGetMe = BotGetMe
  { _botGetMe_id :: !Word64
  , _botGetMe_isBot :: !Bool
  , _botGetMe_firstName :: !Text
  , _botGetMe_username :: !Text
  } deriving (Eq, Ord, Show, Typeable, Generic)

data BotGetUpdates = BotGetUpdates
  { _botGetUpdates_updateId :: !Word64
  , _botGetUpdates_message :: !BotMessage
  } deriving (Eq, Ord, Show, Typeable, Generic)

data BotMessage = BotMessage
  { _botMessage_messageId :: !Word64
  , _botMessage_from :: !Sender -- This is optional in the spec, but we will require it
  , _botMessage_chat :: !Chat
  , _botMessage_text :: !(Maybe Text)
  , _botMessage_date :: !UnixTimestamp
  } deriving (Eq, Ord, Show, Typeable, Generic)

data Sender = Sender
  { _sender_id :: !Word64
  , _sender_isBot :: !Bool
  , _sender_firstName :: !Text
  , _sender_lastName :: !(Maybe Text)
  , _sender_username :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Typeable, Generic)

data Chat = Chat
  { _chat_id :: !Word64
  , _chat_type :: !Text
  , _chat_title :: !(Maybe Text)
  , _chat_firstName :: !(Maybe Text)
  , _chat_lastName :: !(Maybe Text)
  , _chat_username :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Typeable, Generic)

newtype SendMessageResult = SendMessageResult
  { _sendMessage_message_id :: Word64
  } deriving (Eq, Ord, Show, Typeable, Generic)

getMe
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => Text -> m (ApiResult BotGetMe)
getMe cfg = do
  $(logDebug) "Getting Telegram bot metadata"
  fmap Http.getResponseBody $
    Http.req . Http.Request_JSON =<< Http.parseRequest (maybe "" (T.unpack . Uri.render) $ telegramApiGetMeUri cfg)

sendMessage
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => Text -> SendMessageRequest -> m (ApiResult SendMessageResult)
sendMessage botApiKey cfg = do
  $(logDebug) $ "Sending a telegram message: " <> tshow cfg
  fmap Http.getResponseBody $
    Http.req . Http.Request_JSON =<<
      Http.setRequestBodyJSON cfg . Http.setRequestMethod "POST" <$>
        Http.parseRequest (maybe "" (T.unpack . Uri.render) $ telegramApiSendMessageUri botApiKey)

getUpdates
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => TelegramGetUpdates -> m (ApiResult [BotGetUpdates])
getUpdates cfg = do
  $(logDebug) $ "Getting updates for Telegram starting at offset " <> tshow (_telegramGetUpdates_offset cfg)
  fmap Http.getResponseBody $
    Http.req . Http.Request_JSON
      =<< setTimeout <$> Http.parseRequest (maybe "" (T.unpack . Uri.render) $ telegramApiGetUpdatesUri cfg)
  where
    setTimeout req = req { Http.responseTimeout = Http.responseTimeoutMicro $
      fromIntegral $ nominalDiffTimeToMicroseconds $ _telegramGetUpdates_timeout cfg + 1 }

waitForFirstSender
  :: MonadIO m => (MonadThrow m, MonadLogger m)
  => Http.Manager
  -> IO (Maybe (Id TelegramConfig, TelegramConfig))
  -> m (Maybe (Id TelegramConfig, BotMessage))
waitForFirstSender httpMgr getTelegramCfg = begin Nothing
  where
    begin firstMessageId = liftIO getTelegramCfg >>= \case
      Nothing -> pure Nothing
      Just cfg -> runApi cfg firstMessageId

    startOver n = do
      threadDelay' 1
      begin ((+1) <$> n) -- Increment the first message ID if we actually have a starting point

    runApi (cid, telegramCfg) firstMessageId = do
      result <- runHttpT httpMgr $ getUpdates TelegramGetUpdates
        { _telegramGetUpdates_botApiKey = _telegramConfig_botApiKey telegramCfg
        , _telegramGetUpdates_offset = firstMessageId
        , _telegramGetUpdates_timeout = 60
        }
      case _apiResult_ok result of
        False -> $(logWarn) "Telegram API result NOT ok" *> startOver firstMessageId
        True -> do
          let
            botUpdated = _telegramConfig_updated telegramCfg
            messages = _botGetUpdates_message <$> _apiResult_result result
            candidateMessages =
              filter (\msg ->
                  unUnixTimestamp (_botMessage_date msg) >= botUpdated && -- Message must occur after the bot config was changed
                  not (_sender_isBot (_botMessage_from msg))  -- Message cannot come from a bot
                ) messages
          case null candidateMessages of
            True -> startOver $ maximumMay $ map _botGetUpdates_updateId $ _apiResult_result result
            False -> pure $
              fmap ((,) cid) $ minimumByMay (comparing _botMessage_date) messages

telegramWorker
  :: forall m. (MonadIO m)
  => Http.Manager
  -> LoggingEnv
  -> Pool Postgresql
  -> IO () -- Signal that blocks until the worker should start again.
  -> m (IO ())
telegramWorker httpMgr loggingEnv db signal = worker' $ do
  withAsync waitForSender $ const $ do
    signal

  where
    getTelegramCfg = fmap listToMaybe $ runLoggingEnv loggingEnv $ runDb (Identity db) $ do
      cfgs <- selectIds TelegramConfigConstructor ((TelegramConfig_enabledField ==. True) `limitTo` 1)
      recipients <- project TelegramRecipient_deletedField $
        (TelegramRecipient_configField `in_` map fst cfgs &&. TelegramRecipient_deletedField ==. False)
        `limitTo` 1
      pure $ if null recipients then cfgs else [] -- Only return this config if it doesn't have any recipients yet.

    waitForSender = runLoggingEnv loggingEnv $ waitForFirstSender httpMgr getTelegramCfg >>= \case
      Nothing -> $(logDebug) "Didn't find any new Telegram recipients"
      Just (cid, message) -> runDb (Identity db) $ do
        rid' <- fmap toId . listToMaybe <$> project AutoKeyField (TelegramRecipient_chatIdField ==. _chat_id (_botMessage_chat message))
        now <- getTime
        case rid' of
          Nothing -> do
            let
              recipient = TelegramRecipient
                { _telegramRecipient_config = cid
                , _telegramRecipient_userId = _sender_id $ _botMessage_from message
                , _telegramRecipient_chatId = _chat_id $ _botMessage_chat message
                , _telegramRecipient_firstName = _sender_firstName $ _botMessage_from message
                , _telegramRecipient_lastName = _sender_lastName $ _botMessage_from message
                , _telegramRecipient_username = _sender_username $ _botMessage_from message
                , _telegramRecipient_created = now
                , _telegramRecipient_deleted = False
                }
            notify . flip Notify_TelegramRecipient (Just recipient) =<< insert' recipient
          Just rid -> do
            updateId rid
              [ TelegramRecipient_configField =. cid
              , TelegramRecipient_userIdField =. _sender_id (_botMessage_from message)
              , TelegramRecipient_chatIdField =. _chat_id (_botMessage_chat message)
              , TelegramRecipient_firstNameField =. _sender_firstName (_botMessage_from message)
              , TelegramRecipient_lastNameField =. _sender_lastName (_botMessage_from message)
              , TelegramRecipient_usernameField =. _sender_username (_botMessage_from message)
              , TelegramRecipient_createdField =. now
              , TelegramRecipient_deletedField =. False
              ]
            notify . Notify_TelegramRecipient rid =<< getId rid

emptyTelegramMessageQueue
  :: Http.Manager
  -> LoggingEnv
  -> Pool Postgresql
  -> IO (IO ())
emptyTelegramMessageQueue httpMgr logger db = workerWithDelay (pure 1) $ const $ runLoggingEnv logger $ do
  messages <- runDb (Identity db) [queryQ|
    SELECT tmq.id, tmq.message, tr."chatId", tc."botApiKey"
    FROM "TelegramMessageQueue" tmq
    JOIN "TelegramRecipient" tr ON tr.id = tmq.recipient
    JOIN "TelegramConfig" tc ON tc.id = tr.config
    WHERE tc.enabled AND NOT tr.deleted
  |]
  for_ messages $ \(tmqId :: Id TelegramMessageQueue, tmqMessage, trChatId, tcBotApiKey) -> do
    -- TODO: Logging
    _ <- runHttpT httpMgr $ sendMessage tcBotApiKey SendMessageRequest
      { _sendMessageRequest_chatId = trChatId
      , _sendMessageRequest_text = tmqMessage
      , _sendMessageRequest_parseMode = Nothing
      }
    runDb (Identity db) [executeQ|DELETE FROM "TelegramMessageQueue" where id = ?tmqId|]


concat <$> traverse (deriveJSON $ defaultTezosCompatJsonOptions { Aeson.omitNothingFields = True })
  [ 'ApiResult
  , 'BotGetMe
  , 'BotGetUpdates
  , 'BotMessage
  , 'Chat
  , 'Sender
  , 'SendMessageRequest
  , 'SendMessageResult
  ]

concat <$> traverse makeLenses
  [ ''ApiResult
  , ''BotGetMe
  , ''BotGetUpdates
  , ''BotMessage
  , ''Chat
  , ''Sender
  , ''SendMessageRequest
  , ''SendMessageResult
  , ''TelegramGetUpdates
  ]
