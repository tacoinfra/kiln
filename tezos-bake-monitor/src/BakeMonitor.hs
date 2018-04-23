{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Snap
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Lens.Combinators (over, set, makeLenses, _head)
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import Control.Concurrent.MVar
import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Monad.Trans
import Data.Monoid ((<>), mempty)
import Safe (headDef)
import Options.Applicative

-- Type for common sorts of categories of message that the baker emits while baking.
-- We're mainly interested in counting the blocks that are injected, but some of the rest is potentially useful.
data MessageType =
    MessageType_Selected   -- Select candidate block after BKidogWLoxoM (slot 1) fitness: 00::00000000000000df
  | MessageType_Injected Text -- Injected block BKiNQABfPLcg for my-ident after BKiNpAqXuqEx  (level 222, slot 0, fitness 00::00000000000000de, operations 0+0+0+0)
  | MessageType_NoNonce    -- No nonce to reveal for block BKiNmhYodVSR
  | MessageType_Error      -- Error while endorsing:
  | MessageType_ErrorCont  -- Error, dumping error stack:
                           --   Wrong predecessor BKiQtCxSQRGcr3QPX7RR62VNnM9DXNv4NnJ6HjJso3f9W87FLHT, expected BKiUhCVyeftvggKDw3UjHhXSXdmUnV4epidsyHnS5B2XEA2gcEh
                           -- ^^ note these two spaces.
  | MessageType_Unknown    -- Anything else.

classify :: Text -> MessageType
classify t
  | "Select candidate block" `T.isPrefixOf` t = MessageType_Selected
  | "Injected block " `T.isPrefixOf` t = MessageType_Injected . T.takeWhile (/= ' ')  . T.drop (T.length "Injected block ") $ t
  | "No nonce to reveal" `T.isPrefixOf` t = MessageType_NoNonce
  | "error stack:" `T.isSuffixOf` t = MessageType_ErrorCont
  | "Error" `T.isPrefixOf` t = MessageType_Error
  | "  " `T.isPrefixOf` t = MessageType_ErrorCont
  | otherwise = MessageType_Unknown

data Count = Count
  { _count_selected :: !Integer
  , _count_injected :: !Integer
  , _count_errors :: !Integer
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Count
instance ToJSON Count

makeLenses 'Count

data Baked = Baked
  { _baked_seq :: !Integer
  , _baked_hash :: Text
  , _baked_time :: UTCTime
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Baked
instance ToJSON Baked

makeLenses 'Baked

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Error
instance ToJSON Error

makeLenses 'Error

data Top = Top
  { _top_counts :: Count
  , _top_last_baked :: [Baked]
  , _top_errors :: [Error]
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Top
instance ToJSON Top

makeLenses 'Top

baked_horizon = 20
error_horizon = 20

opts :: Parser (IO ())
opts = mainArgs
  <$> option auto
      (  long "port"
      <> short 'p'
      <> help "Port to listen on"
      <> showDefault
      <> value 9800
      <> metavar "PORT"
      )
  <*> argument str
      (  metavar "CLIENT"
      )
  <*> many (argument str (metavar "ARGS..."))

main = join $ do
  customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (opts <**> helper) idm)

mainArgs port x xs = do
  (_, Just out, _, ph) <- createProcess (proc x xs)
    { std_out = CreatePipe
    }
  started <- getCurrentTime
  dataRef <- newMVar $ Top
    (Count 0 0 0)
    []
    []
  forkIO . forever $ do
    msg <- T.hGetLine out
    now <- getCurrentTime
    case classify msg of
      MessageType_Selected -> modifyMVar_ dataRef $ return . over (top_counts . count_selected) (+1)
      MessageType_Injected h ->
        modifyMVar_ dataRef $ return
          . over (top_counts . count_injected) (+1)
          . over top_last_baked (\bs -> take 20 $
              ( over baked_seq (+1)
              . set baked_hash h
              . set baked_time now
              $ headDef (Baked 0 h now) bs)
            : bs)
      MessageType_Error ->
        modifyMVar_ dataRef $ return
          . over (top_counts . count_errors) (+1)
          . over top_errors (take 20 . (Error now msg :))
      MessageType_ErrorCont ->
        modifyMVar_ dataRef $ return
          . over (top_errors . _head . error_text) (`T.append` msg)
      _ -> return ()
    T.putStrLn (T.pack (show now) <> ": " <> msg)
  let rootHandler = do
        c <- liftIO $ readMVar dataRef
        writeLBS (encode c)

  httpServe (setPort port mempty) rootHandler
