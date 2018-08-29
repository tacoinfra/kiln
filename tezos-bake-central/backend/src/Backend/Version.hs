{-# LANGUAGE CPP #-}

module Backend.Version (version, parseVersion) where

import Data.Function (on)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Version as V
import Text.ParserCombinators.ReadP (readP_to_S)


#if defined(REAL_BUILD)

import qualified Paths_backend

version :: V.Version
version = Paths_backend.version

#else

version :: V.Version
version = V.makeVersion [0]

#endif

parseVersion :: Text -> Maybe V.Version
parseVersion txt =
  -- Pick the parse that has nothing leftover
  fmap fst $ List.find (null . snd) $ readP_to_S V.parseVersion (T.unpack txt)
