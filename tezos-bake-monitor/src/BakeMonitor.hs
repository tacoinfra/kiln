{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Snap
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Lens.Combinators
import Data.Aeson
import GHC.Generics
import Control.Concurrent.MVar
import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Monad.Trans
import Data.Monoid

-- Type for common sorts of categories of message that the baker emits while baking.
-- We're mainly interested in counting the blocks that are injected, but some of the rest is potentially useful.
data MessageType =
    MessageType_Selected   -- Select candidate block after BKidogWLoxoM (slot 1) fitness: 00::00000000000000df
  | MessageType_Injected   -- Injected block BKiNQABfPLcg for my-ident after BKiNpAqXuqEx  (level 222, slot 0, fitness 00::00000000000000de, operations 0+0+0+0)
  | MessageType_NoNonce    -- No nonce to reveal for block BKiNmhYodVSR
  | MessageType_Error      -- Error while endorsing:
  | MessageType_StackHead  -- Error, dumping error stack:
  | MessageType_StackEntry --   Wrong predecessor BKiQtCxSQRGcr3QPX7RR62VNnM9DXNv4NnJ6HjJso3f9W87FLHT, expected BKiUhCVyeftvggKDw3UjHhXSXdmUnV4epidsyHnS5B2XEA2gcEh
                           -- ^^ note these two spaces.
  | MessageType_Unknown    -- Anything else.

classify :: Text -> MessageType
classify t
  | "Select candidate block" `T.isPrefixOf` t = MessageType_Selected
  | "Injected block" `T.isPrefixOf` t = MessageType_Injected
  | "No nonce to reveal" `T.isPrefixOf` t = MessageType_NoNonce
  | "error stack:" `T.isSuffixOf` t = MessageType_StackHead
  | "Error" `T.isPrefixOf` t = MessageType_Error
  | "  " `T.isPrefixOf` t = MessageType_StackEntry
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

main = do
  args <- getArgs
  case args of
    (x:xs) -> mainArgs x xs
    [] -> putStrLn "Usage: tezos-bake-monitor <tezos-client commandline...>"

mainArgs x xs = do
  (_, Just out, _, ph) <- createProcess (proc x xs)
    { std_out = CreatePipe
    }
  countRef <- newMVar (Count 0 0 0)
  forkIO . forever $ do
    msg <- T.hGetLine out
    case classify msg of
      MessageType_Selected -> modifyMVar_ countRef $ return . over count_selected (+1)
      MessageType_Injected -> modifyMVar_ countRef $ return . over count_injected (+1)
      MessageType_Error -> modifyMVar_ countRef $ return . over count_errors (+1)
      _ -> return ()
    now <- getCurrentTime
    T.putStrLn (T.pack (show now) <> ": " <> msg)
  let rootHandler = do
        c <- liftIO $ readMVar countRef
        writeLBS (encode c)

  httpServe (setPort 9800 mempty) rootHandler