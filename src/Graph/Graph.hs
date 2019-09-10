{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Graph.Graph
  ( Graph
  , Node
  , NodeField
  , NodeLink
  , NodeId(..)
  , mkNode
  , mkNodeId
  , mkNodeLink
  , mkGraph
  , mkNodeField
  , renderGraph
  ) where

import GHC.Generics
import Data.Text as T
import Data.Maybe as M
import Data.List as L

mkNode = Node
mkNodeField = NodeField
mkGraph = Graph
mkNodeLink = NodeLink

mkNodeId :: String -> NodeId
mkNodeId = NodeId . L.filter (/= ' ')


data Node =
  Node { nodeName :: String
       , nodeId :: NodeId
       , nodeFields :: [NodeField]
       , nodeColor :: String
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
          "<table border='0' cellborder='1' color='"++ nodeColor node ++"' cellspacing='0'>\n"
          ++ tableHead
          ++ (subTable $ nodeFields node)
          ++ "</table>\n"
    subTable fields =
      case tableFields fields of
        [] -> ""
        fields -> "<tr><td><table border='0' color='orange' cellspacing='0' cellborder='0'>\n"
          ++ fields
          ++ "</table></td></tr>\n"
    tableFields = L.intercalate "\n" . L.map mkField
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
