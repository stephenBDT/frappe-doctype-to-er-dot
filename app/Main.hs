{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import Data.Text as T
import Data.List as L
import Data.Aeson as JSON
import System.Directory
import GHC.Generics
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Pangraph.GraphML.Writer
import Pangraph


getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

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
-- decode :: FromJSON a => ByteString -> Maybe a

testf:: Text -> Text
testf = id

testPath:: String
testPath = "./frappe/frappe/printing/doctype/"


getSubDirs:: FilePath -> IO [FilePath]
getSubDirs fp =
  (Prelude.map (fp ++) <$> listDirectory fp)
  >>= (Control.Monad.filterM doesDirectoryExist)

findDocTypes:: FilePath -> IO [FilePath]
findDocTypes fp =
  do
    jsonsHere <- Prelude.filter isJson <$> L.map mkFullPath <$> listDirectory fp
    subDirs <- getSubDirs fp
    deeperJsons <- L.concat <$> mapM findDocTypes subDirs
    return $ jsonsHere ++ deeperJsons
  where
    mkFullPath p = fp ++ "/" ++ p
    isJson s = L.isSuffixOf ".json" s


getDocTypeFromJson :: FilePath -> IO (Either String DocType)
getDocTypeFromJson path = getJSON path >>= return . JSON.eitherDecode
 -- t <- findDocTypes testPath >>= (mapM getJSON ) >>= return . L.head >>= \t -> return $ (JSON.eitherDecode t :: Either String DocType)

getAllDoctypes:: FilePath -> IO [Either String DocType]
getAllDoctypes fp = findDocTypes fp >>= mapM getDocTypeFromJson 

test =
  L.concat <$>
  (listDirectory testPath
    >>= (return . L.map (testPath ++))
      >>= filterM doesDirectoryExist
        >>= mapM (\fp -> L.map (fp ++) <$> listDirectory fp))

main :: IO ()
main = do
  currentDir <- listDirectory testPath
  _ <- mapM putStrLn currentDir
  return ()
