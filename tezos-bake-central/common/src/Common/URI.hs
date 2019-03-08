{-# LANGUAGE OverloadedStrings #-}

module Common.URI where

import Control.Applicative (liftA2)
import Control.Lens ((%~), (.~))
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Word (Word16)
import qualified Text.URI as Uri
import qualified Text.URI.Lens as UriL

type Port = Word16

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

appendQueryParams :: Uri.URI -> [(Text, Text)] -> Maybe Uri.URI
appendQueryParams uri qargs = do
  qs <- for qargs $ \(k, v) -> liftA2 Uri.QueryParam (Uri.mkQueryKey k) (Uri.mkQueryValue v)
  Just $ uri & UriL.uriQuery %~ (<> qs)
