{-# LANGUAGE OverloadedStrings #-}

module Common.URI where

import Control.Lens ((%~), (.~))
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.URI as Uri
import qualified Text.URI.Lens as UriL

mkRootUri :: Text -> Either Text Uri.URI
mkRootUri txt = case Uri.mkURI $ T.strip txt of
  Nothing -> Left "URI is invalid"
  Just uri
    | not $ Uri.isPathAbsolute uri -> Left "URI must be absolute"
    | not $ null $ Uri.uriQuery uri -> Left "URI must not have query parameters"
    | not $ null $ Uri.uriFragment uri -> Left "URI must not have a fragment"
    | otherwise -> Right $ uri & UriL.uriTrailingSlash .~ False

appendPaths :: Uri.URI -> [Text] -> Maybe Uri.URI
appendPaths uri paths = do
  pieces <- traverse Uri.mkPathPiece paths
  Just $ uri & UriL.uriPath %~ (<> pieces)
