{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE LambdaCase#-}
module Tezos.NodeRPC.Schema where

import Control.Applicative
import Data.Aeson
import Data.Maybe(catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import JSONSchema.Draft4 (Schema(..))

-- TODO:  i'm not sure this is really useful, but it's at least convenient for now
class FoldServices a where
  getServices :: a -> [([PathItem], Maybe Service)]

data DirectoryDescr
  = Static StaticDirectory
  | Dynamic (Maybe Text)
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON DirectoryDescr where
  parseJSON = withObject "DirectoryDescr" $ \v ->
          (Dynamic <$> v .: "dynamic")
      <|> (Static <$> v .: "static")


instance FoldServices DirectoryDescr where
  getServices (Static s) = getServices s
  getServices (Dynamic d) = maybe [] (\x -> [([PDynamic $ Arg x Nothing],Nothing)]) d

data StaticDirectory = StaticDirectory
  { _staticDirectoryService_getService :: Maybe Service
  , _staticDirectoryService_postService :: Maybe Service
  , _staticDirectoryService_deleteService :: Maybe Service
  , _staticDirectoryService_putService :: Maybe Service
  , _staticDirectoryService_patchService :: Maybe Service
  , _staticDirectoryService_subdirs :: Maybe StaticSubdirectories
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON StaticDirectory where
  parseJSON = withObject "StaticDirectory" $ \v -> StaticDirectory
     <$> v .:? "get_service"
     <*> v .:? "post_service"
     <*> v .:? "delete_service"
     <*> v .:? "put_service"
     <*> v .:? "patch_service"
     <*> v .:? "subdirs"

instance FoldServices StaticDirectory where
  getServices sd = these <> those
    where
      these = maybe [] getServices $ _staticDirectoryService_subdirs sd
      those = (fmap.fmap) Just $ mapMaybe (\f -> (,) <$> pure [] <*> f sd)
        [ _staticDirectoryService_getService
        , _staticDirectoryService_postService
        , _staticDirectoryService_deleteService
        , _staticDirectoryService_putService
        , _staticDirectoryService_patchService
        ]

data StaticSubdirectories
  = StaticSubdirectories_Suffixes [StaticSubdirsSuffixes]
  | StaticSubdirectories_Arg StaticSubdirsDynamic
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON StaticSubdirectories where
  parseJSON = withObject "StaticSubdirectories" $ \v ->
        (StaticSubdirectories_Arg <$> v .: "dynamic_dispatch")
     <|>(StaticSubdirectories_Suffixes <$> v .: "suffixes")

instance FoldServices StaticSubdirectories where
  getServices (StaticSubdirectories_Suffixes xs) = getServices =<< xs
  getServices (StaticSubdirectories_Arg x) = getServices x

data StaticSubdirsSuffixes = StaticSubdirsSuffixes
  { _staticSubdirsSuffixes_name :: Text
  , _staticSubdirsSuffixes_tree :: DirectoryDescr
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON StaticSubdirsSuffixes where
  parseJSON = withObject "StaticSubdirsSuffixes" $ \v -> StaticSubdirsSuffixes
    <$> v .: "name"
    <*> v .: "tree"
instance FoldServices StaticSubdirsSuffixes where
  getServices x = (\(p, s) -> (PStatic (_staticSubdirsSuffixes_name x) : p, s)) <$> (getServices $ _staticSubdirsSuffixes_tree x)

data StaticSubdirsDynamic = StaticSubdirsDynamic
  { _staticSubdirsDynamic_arg :: Arg
  , _staticSubdirsDynamic_tree :: DirectoryDescr
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON StaticSubdirsDynamic where
  parseJSON = withObject "StaticSubdirsDynamic" $ \v -> StaticSubdirsDynamic
    <$> v .: "arg"
    <*> v .: "tree"

instance FoldServices StaticSubdirsDynamic where
  getServices x = (\(p, s) -> (PDynamic dname : p, s)) <$> (getServices $ _staticSubdirsDynamic_tree x)
    where
      dname = _staticSubdirsDynamic_arg x

data Method
  = GET
  | POST
  | DELETE
  | PUT
  | PATCH
  deriving (Eq, Show, Typeable, Generic, Enum)
instance FromJSON Method

data Service = Service
  { _service_meth :: Method
  , _service_path :: [PathItem]
  , _service_description :: Maybe Text
  , _service_query :: [QueryItem]
  , _service_input :: Maybe TezosSchema
  , _service_output :: TezosSchema
  , _service_error :: TezosSchema
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON Service where
  parseJSON = withObject "Service" $ \v -> Service
    <$> v .: "meth"
    <*> v .: "path"
    <*> v .:? "description"
    <*> v .: "query"
    <*> v .:? "input"
    <*> v .: "output"
    <*> v .: "error"

data TezosSchema = TezosSchema
  { _tezosSchema_jsonSchema :: Schema
  , _tezosSchema_binarySchema :: BinarySchema
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON TezosSchema where
  parseJSON = withObject "TezosSchema" $ \v -> TezosSchema
    <$> v .: "json_schema"
    <*> v .: "binary_schema"

-- TODO
data BinarySchema = BinarySchema
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON BinarySchema where
  parseJSON _ = pure BinarySchema

data PathItem
  = PStatic Text
  | PDynamic Arg
  | PDynamicTail Arg
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON PathItem where
  parseJSON v
    =   (PStatic <$> parseJSON v)
    <|> (withObject "PathItem" $ \v' -> do
          v' .: "id" >>= \case
            "single" -> PDynamic <$> parseJSON v
            "multiple" -> PDynamicTail <$> parseJSON v
            bad -> fail ("not a pathitem" <> bad)
        ) v

data Arg = Arg
  { _arg_name :: Text
  , _arg_descr :: Maybe Text
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON Arg where
  parseJSON = withObject "Arg" $ \v -> Arg
    <$> v .: "name"
    <*> v .:? "descr"

data QueryItem = QueryItem
  { _queryItem_name :: Text
  , _queryItem_description :: Maybe Text
  , _queryItem_kind :: QueryItemKind
  }
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON QueryItem where
  parseJSON = withObject "QueryItem" $ \v -> QueryItem
    <$> v .: "name"
    <*> v .:? "description"
    <*> v .: "kind"

data QueryItemKind
  = QueryItemKind_Single Arg
  | QueryItemKind_Optional Arg
  | QueryItemKind_Flag Unit
  | QueryItemKind_Multi Arg
  deriving (Eq, Show, Typeable, Generic)

data Unit = Unit
  deriving (Eq, Show, Typeable, Generic)
instance FromJSON Unit where
  parseJSON _ = pure Unit

instance FromJSON QueryItemKind where
  parseJSON = withObject "QueryItemKind" $ \v ->
        (QueryItemKind_Single <$> v .: "single")
    <|> (QueryItemKind_Multi <$> v .: "multi")
    <|> (QueryItemKind_Optional <$> v .: "optional")
    <|> (QueryItemKind_Flag <$> v .: "flag")
