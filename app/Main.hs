{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import Data.Text as T
import Data.Maybe as M
import Data.List as L
import Data.Aeson as JSON
import System.Directory
import GHC.Generics
import Control.Monad
import Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS

data Node =
  Node { nodeName :: String
       , nodeId :: NodeId
       , nodeFields :: [NodeField]
       } deriving (Show, Eq, Ord)

data NodeField =
  NodeField { fieldName :: String
            , fieldId :: String -- TODO (NodeId, String) --NodeName in CamelCase and field_name in python notation
            , fieldType :: String
            , fieldLink :: Maybe String
            } deriving (Show, Eq, Ord)

data NodeLink =
  NodeLink
  { sourceId :: String
  , targetId :: NodeId
  } deriving (Show, Eq, Ord)

data Graph = Graph [Node] [NodeLink]

newtype NodeId =
  NodeId {unNodeId :: String
         } deriving(Show, Eq, Ord)

renderNode :: Node -> String
renderNode node = (unNodeId $ nodeId node) ++ "[\nshape=plaintext\nlabel=<" ++ mainTbl ++ ">];"
  where
    tableName = nodeName node
    tableHead =
      "<tr><td>" ++ tableName ++ "</td></tr>\n"
    mainTbl =
          "<table border='0' cellborder='1' color='blue' cellspacing='0'>\n"
          ++ tableHead
          ++ subTable
          ++ "</table>\n"
    subTable =
      "<tr><td><table border='0' color='orange' cellspacing='0' cellborder='0'>\n"
      ++ tableFields
      ++ "</table></td></tr>\n"
    tableFields = L.intercalate "\n" $ L.map mkField $ nodeFields node
    mkField f =
      "<tr><td align='left'>" ++ fieldName f
      ++ "</td><td align='left'  port='" ++fieldId f ++ "'>:" ++ fieldType f
      ++ "</td></tr>\n"

renderLink :: NodeLink -> String
renderLink (NodeLink from (NodeId to)) = from ++ " -> " ++ to

renderGraph :: Graph -> String
renderGraph (Graph nodes links) =
  "digraph models_diagram {\n graph[overlap=false, splines=true]"
  ++ L.intercalate "\n\n" (L.map renderNode nodes)
  ++ L.intercalate "\n" (L.map renderLink links)
  ++ "}"

docListToGraph :: [DocType] -> Graph
docListToGraph list =
  let (nodes, links) = docListToNodes list
  in Graph nodes $ L.concat links

docListToNodes :: [DocType] -> ([Node], [[NodeLink]])
docListToNodes docs =
  let nodeId x = mkNodeId $ name x
  in unzip $ L.map (\x -> (Node { nodeName = name x
                             , nodeId = nodeId x
                             , nodeFields = M.mapMaybe (mkNodeField $ nodeId x) $ fields x},
                         getLinks (nodeId x) $ fields x)) docs

mkNodeId :: String -> NodeId
mkNodeId = NodeId . L.filter (/= ' ')

getLinks :: NodeId -> [DocField] -> [NodeLink]
getLinks nodeId fields =
  L.concat $
  L.map (\field ->
           M.maybeToList $ NodeLink (unNodeId nodeId ++ ":" ++ fieldname field) . mkNodeId <$> options field) $
  L.filter (("Link" ==) . fieldtype) fields

mkNodeField :: NodeId -> DocField -> Maybe NodeField
mkNodeField nodeId x =
  case fieldtype x of
    "Section Break" -> Nothing
    "Column Break" -> Nothing
    _ -> Just $ 
      NodeField { fieldName = fieldname x
                , fieldId  = fieldname x
                , fieldType = fieldtype x
                , fieldLink = if fieldtype x == "Link" then options x else Nothing
                }

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

findDocTypesInDir dir =
  L.concat <$>
  (listDirectory dir
    >>= (return . L.map (testPath ++))
      >>= filterM doesDirectoryExist
        >>= mapM (\fp -> L.map (fp ++) <$> listDirectory fp))

test =
  L.concat <$>
  (listDirectory testPath
    >>= (return . L.map (testPath ++))
      >>= filterM doesDirectoryExist
        >>= mapM (\fp -> L.map (fp ++) <$> listDirectory fp))

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error "not a just my firend"

renderDocTypes :: FilePath -> IO ()
renderDocTypes fp =
  do
    docTypes <- getAllDoctypes fp
    writeFile "out.dot" $ renderGraph $ docListToGraph $ M.mapMaybe (\x -> case x of
                                                                    Left _ -> Nothing
                                                                    Right a -> Just a) docTypes


  
main :: IO ()
main = do
  currentDir <- listDirectory testPath
  _ <- mapM putStrLn currentDir
  renderDocTypes testPath

