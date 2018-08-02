{-# LANGUAGE CPP #-}

module Backend.Version (version, parseVersion) where

import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Version as V
import Safe (maximumByMay)
import Text.ParserCombinators.ReadP (readP_to_S)


#if defined(REAL_BOY)

import qualified Paths_backend

version :: V.Version
version = Paths_backend.version

#else

version :: V.Version
version = V.makeVersion [0]

#endif

parseVersion :: Text -> Maybe V.Version
parseVersion txt =
  maximumByMay (compare `on` length . V.versionBranch) $ -- Pick the parse that has the most number components
    fst <$> readP_to_S V.parseVersion (T.unpack txt)
