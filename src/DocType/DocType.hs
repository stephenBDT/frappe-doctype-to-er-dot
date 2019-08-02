{-# LANGUAGE DeriveGeneric #-}
module DocType.DocType where

import Data.Text as T
import Data.List as L
import Data.Aeson as JSON
import System.Directory
import GHC.Generics
import Control.Monad
import Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS

fromBs :: BS.ByteString -> String
fromBs = BSU.toString

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

getDocTypeFromJson :: FilePath -> IO (Either String DocType)
getDocTypeFromJson path = getJSON path >>= return . JSON.eitherDecode

data DocField =
  DocField { fieldname:: String
           , fieldtype:: String
           , label:: Maybe String
           , options:: Maybe String
           } deriving (Show, Generic)

data Permission =
  Permission { role:: String } deriving (Show, Generic)

data DocType =
  DocType { fields:: [DocField]
          , isTable:: Maybe Int
          -- , module:: String
          , name:: String
          , permissions:: [Permission]
          } deriving (Show, Generic)

instance FromJSON DocField
instance ToJSON DocField
instance FromJSON Permission
instance ToJSON Permission
instance FromJSON DocType
instance ToJSON DocType
