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

import Control.Concurrent (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (withAsync)
import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
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
import Safe (maximumByMay, maximumMay, minimumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Backend.Common (threadDelay', worker', workerWithDelay)
import Backend.Http (HasHttp, runHttpT)
import qualified Backend.Http as Http
import Backend.Schema
import Common (defaultTezosCompatJsonOptions, nominalDiffTimeToMicroseconds, nominalDiffTimeToSeconds,
               unixEpoch)
import Common.Schema
import Common.URI as Uri (appendPaths, appendQueryParams)
import ExtraPrelude

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
      { _env_beginWaitingForNewRecipient = void $ tryPutMVar newRecipientChan ()
      , _env_waitForNewRecipient = takeMVar newRecipientChan
      }
  -- addFinalizer =<< telegramWorker httpMgr logger db (_env_waitForNewRecipient env)
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
  , _telegramGetUpdates_offset :: !(Maybe Int64) -- ID of first message to show
  , _telegramGetUpdates_timeout :: !(Maybe NominalDiffTime) -- Long-polling timeout
  } deriving (Eq, Ord, Show, Typeable, Generic)

telegramApiGetUpdatesUri :: TelegramGetUpdates -> Maybe URI
telegramApiGetUpdatesUri cfg =
  telegramApiBotUri (_telegramGetUpdates_botApiKey cfg)
  >>= flip appendPaths ["getUpdates"]
  >>= flip appendQueryParams
    (  [ ("offset", T.pack $ UriEncode.encode $ show offset) | Just offset <- [_telegramGetUpdates_offset cfg] ]
    <> [ ("timeout", T.pack $ UriEncode.encode $ show $ nominalDiffTimeToSeconds timeout) | Just timeout <- [_telegramGetUpdates_timeout cfg]]
    <> [ ("allowed_updates", "messages") ]
    )

data SendMessageRequest = SendMessageRequest
  { _sendMessageRequest_chatId :: !Int64
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

-- | Type that only succeeds JSON parsing if it is 'True', not 'False'.
data OnlyTrue = OnlyTrue deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON OnlyTrue where
  parseJSON a = bool (fail "Value was false") (pure OnlyTrue) =<< Aeson.parseJSON a
instance ToJSON OnlyTrue where
  toJSON OnlyTrue = Aeson.toJSON True
  toEncoding OnlyTrue = Aeson.toEncoding True

data ApiResult a = ApiResult
  { _apiResult_ok :: !OnlyTrue
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
  , _botMessage_new_chat_member :: !(Maybe BotGetMe)
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
  { _chat_id :: !Int64
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
    Http.req . Http.Request_JSON =<< Http.parseRequestThrow (maybe "" (T.unpack . Uri.render) $ telegramApiGetMeUri cfg)

sendMessage
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => Text -> SendMessageRequest -> m (ApiResult SendMessageResult)
sendMessage botApiKey cfg = do
  $(logDebug) $ "Sending a telegram message: " <> tshow cfg
  fmap Http.getResponseBody $
    Http.req . Http.Request_JSON =<<
      Http.setRequestBodyJSON cfg . Http.setRequestMethod "POST" <$>
        Http.parseRequestThrow (maybe "" (T.unpack . Uri.render) $ telegramApiSendMessageUri botApiKey)

getUpdates
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => TelegramGetUpdates -> m (ApiResult [BotGetUpdates])
getUpdates cfg = do
  $(logDebug) $ "Getting updates for Telegram starting at offset " <> tshow (_telegramGetUpdates_offset cfg)
  fmap Http.getResponseBody $
    Http.req . Http.Request_JSON
      =<< maybe id setTimeout (_telegramGetUpdates_timeout cfg) <$> Http.parseRequestThrow (maybe "" (T.unpack . Uri.render) $ telegramApiGetUpdatesUri cfg)
  where
    setTimeout timeout req = req { Http.responseTimeout = Http.responseTimeoutMicro $
      fromIntegral $ nominalDiffTimeToMicroseconds $ timeout + 1 }

isFromRecipientMessage :: UTCTime -> BotMessage -> Bool
isFromRecipientMessage oldestMessage msg =
  unUnixTimestamp (_botMessage_date msg) >= oldestMessage &&
  not (_sender_isBot (_botMessage_from msg))  -- Message cannot come from a bot

isAddToGroupMessage :: BotGetMe -> UTCTime -> BotMessage -> Bool
isAddToGroupMessage me oldestMessage msg =
  unUnixTimestamp (_botMessage_date msg) >= oldestMessage &&
  not (_sender_isBot (_botMessage_from msg))  -- Message cannot come from a bot
  && (_botMessage_new_chat_member msg == Just me)

-- | One-shot function for getting a bot and its first sender.
getBotAndLastSender
  :: (MonadThrow m, HasHttp m, MonadLogger m)
  => Text -> m (Maybe (BotGetMe, Chat, Sender))
getBotAndLastSender botApiKey = do
  me <- _apiResult_result <$> getMe botApiKey
  result <- getUpdates TelegramGetUpdates
    { _telegramGetUpdates_botApiKey = botApiKey
    , _telegramGetUpdates_offset = Nothing
    , _telegramGetUpdates_timeout = Nothing
    }
  let
    candidateMessages = filter (\m -> isAddToGroupMessage me unixEpoch m || isFromRecipientMessage unixEpoch m)
      $ _botGetUpdates_message <$> _apiResult_result result
    firstMessage = maximumByMay (comparing _botMessage_date) candidateMessages

  pure $ (,,) <$> Just me <*> (_botMessage_chat <$> firstMessage) <*> (_botMessage_from <$> firstMessage)

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
