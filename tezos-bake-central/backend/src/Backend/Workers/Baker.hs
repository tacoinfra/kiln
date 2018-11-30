{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Baker where

import Control.Concurrent.STM (atomically, readTVar, retry)
import Control.Monad (mzero)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (logDebug, logErrorSH)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (execStateT, gets, modify)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..))

import Tezos.NodeRPC
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Common (worker')
import Backend.Schema
import Common.Schema
import ExtraPrelude

bakerWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> m (IO ())
bakerWorker nds = worker' $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  (protoInfo, headM) <- liftIO $ atomically $
    liftA2 (,)
      (maybe retry pure =<< readTVar (_nodeDataSource_parameters nds))
      (dataSourceHead nds)

  let db = _nodeDataSource_pool nds
  res <- runExceptT $ for_ headM $ \headBlock -> flip runReaderT nds $ do
    $(logDebug) "Update baker cycle."
    let
      headHash :: BlockHash = headBlock ^. hash
      headLevel :: RawLevel = headBlock ^. level
      maxLevel = maxRightsLevel protoInfo headLevel
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
    $(logDebug) $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unRawLevel latestCycle)
    bakers :: Map (Id Baker) Baker <- runDb (Identity db) $ selectMap BakerConstructor (Baker_deletedField ==. False)
    let
      bakersSet = Set.fromList $ _baker_publicKeyHash <$> Map.elems bakers

      mkFillMap cacheQ getter = flip execStateT Map.empty $ runMaybeT $ do
        let queries = flip fmap [headLevel..maxLevel] $ \lvl ->
              nodeQueryDataSource $ cacheQ headHash lvl
        for_ queries $ \query -> do
          done <- gets $ (bakersSet `Set.isSubsetOf`) . Map.keysSet
          when done mzero
          stuff <- query
          for_ stuff $ \thing -> do
            modify $ Map.insertWith (\_new old -> old) (getter thing) thing

    bakingRights <- mkFillMap NodeQuery_BakingRights _bakingRights_delegate
    endorsingRights <- mkFillMap NodeQuery_EndorsingRights _endorsingRights_delegate
    runDb (Identity (_nodeDataSource_pool nds)) $ for_ bakers $ \baker -> do
      let pkh = _baker_publicKeyHash baker
      $(logDebug) $ "Updating rights data baker " <> toPublicKeyHashText pkh
      existingIds :: [Id BakerDetails] <- fmap toId <$> project AutoKeyField (BakerDetails_publicKeyHashField ==. pkh)
      let
        newVal = BakerDetails
          { _bakerDetails_publicKeyHash = pkh
          , _bakerDetails_nextBakeRights = _bakingRights_level <$> Map.lookup pkh bakingRights
          , _bakerDetails_nextEndorseRights = _endorsingRights_level <$> Map.lookup pkh endorsingRights
          }
      case nonEmpty existingIds of
        Nothing -> void $ insert newVal
        Just brids -> for_ brids $ \brid -> updateId brid
          [ BakerDetails_nextBakeRightsField =. _bakerDetails_nextBakeRights newVal
          , BakerDetails_nextEndorseRightsField =. _bakerDetails_nextEndorseRights newVal
          ]
      notify $ mkDefaultNotify newVal
  case res of
    Right _ -> pure ()
    Left (err :: RpcError) -> $(logErrorSH) ("bakerWorker" :: String, err)
