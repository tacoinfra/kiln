{-# LANGUAGE CPP #-}

module Backend.Db where

import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Database.Groundhog.Postgresql (Postgresql (..))
import qualified Database.PostgreSQL.Simple as Pg

#if defined(SUPPORT_GARGOYLE)
import qualified Rhyolite.Backend.DB.Gargoyle as GargoyleNix
#endif

gargoyleSupported :: Bool
gargoyleSupported =
#if defined(SUPPORT_GARGOYLE)
  True
#else
  False
#endif

withDb :: Either FilePath Text -> (Pool Postgresql -> IO a) -> IO a
withDb cfg f = case cfg of
  Right connStr -> f =<< openDb (T.encodeUtf8 connStr)
  Left dbPath ->
#   if defined(SUPPORT_GARGOYLE)
        GargoyleNix.withDb dbPath f
#   else
        error "Gargoyle not supported in this build!"
#   endif

openDb :: ByteString -> IO (Pool Postgresql)
openDb dbUri = do
  let
    openPostgresql = Postgresql <$> Pg.connectPostgreSQL dbUri
    closePostgresql (Postgresql p) = Pg.close p
  createPool openPostgresql closePostgresql 1 5 20
