{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Graph.Graph
  ( Graph
  , Node(..)
  , NodeField(..)
  , NodeLink(..)
  , NodeId(..)
  , mkNode
  , mkNodeId
  , mkNodeLink
  , mkGraph
  , mkNodeField
  , renderGraph
  , renderYNode
  , renderYGraph
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
  { sourceId :: (String, Maybe String) -- second being the optional fieldname
  , targetId :: NodeId
  } deriving (Show, Eq, Ord)

data Graph = Graph [Node] [NodeLink]

newtype NodeId =
  NodeId {unNodeId :: String
         } deriving(Show, Eq, Ord)

renderYGraph :: Graph -> String
renderYGraph (Graph nodes edges) =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
\   <graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:java=\"http://www.yworks.com/xml/yfiles-common/1.0/java\" xmlns:sys=\"http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0\" xmlns:x=\"http://www.yworks.com/xml/yfiles-common/markup/2.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">\n\
\     <!--Created by yEd 3.19-->\n\
\     <key attr.name=\"Description\" attr.type=\"string\" for=\"graph\" id=\"d0\"/>\n\
\     <key attr.name=\"description\" attr.type=\"string\" for=\"node\" id=\"d5\"/>\n\
\     <key for=\"node\" id=\"d6\" yfiles.type=\"nodegraphics\"/>\n\
\     <key attr.name=\"url\" attr.type=\"string\" for=\"edge\" id=\"d8\"/>\n\
\     <key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d9\"/>\n\
\     <graph edgedefault=\"directed\" id=\"G\">\n\
\       <data key=\"d0\"/>" ++ L.unlines (L.map renderYNode nodes)  ++ L.unlines (L.map renderYLink (L.zip [1..] edges)) ++
  "      </graph>\n\
\   </graphml>"

renderYNode :: Node -> String
renderYNode (Node nodeName nodeId nodeFields nodeColor) =
    "<node id=\"" ++ unNodeId nodeId ++ "\">\n\
\      <data key=\"d5\"/>\n\
\      <data key=\"d6\">\n\
\        <y:GenericNode configuration=\"com.yworks.entityRelationship.big_entity\">\n\
\          <y:Fill color=\"#E8EEF7\" color2=\"#B7C9E3\" transparent=\"false\"/>\n\
\          <y:BorderStyle color=\"#000000\" type=\"line\" width=\"1.0\"/>\n\
\          <y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" backgroundColor=\"" ++ color ++ "\" configuration=\"com.yworks.entityRelationship.label.name\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasLineColor=\"false\" horizontalTextPosition=\"center\" iconTextGap=\"4\" modelName=\"internal\" modelPosition=\"t\" textColor=\"#000000\" verticalTextPosition=\"bottom\" visible=\"true\"  x=\"20.4833984375\" xml:space=\"preserve\" y=\"4.0\">" ++ nodeName ++ "</y:NodeLabel>" ++ renderYNodeFields nodeFields ++ "\n\
\          <y:StyleProperties>\n\
\            <y:Property class=\"java.lang.Boolean\" name=\"y.view.ShadowNodePainter.SHADOW_PAINTING\" value=\"true\"/>\n\
\          </y:StyleProperties>\n\
\        </y:GenericNode>\n\
\      </data>\n\
\    </node>"
  where
    color = case nodeColor of
      "" -> "#c0c0c0"
      _ -> "#B7C9E3"

renderYNodeFields :: [NodeField] -> String
renderYNodeFields nodefields = -- NodeField fieldName fieldId fieldType fieldLink =
  if L.length nodefields > 0 then
     "<y:NodeLabel alignment=\"left\" autoSizePolicy=\"content\" configuration=\"com.yworks.entityRelationship.label.attributes\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasBackgroundColor=\"false\" hasLineColor=\"false\" height=\"17.96875\" horizontalTextPosition=\"center\" iconTextGap=\"4\" modelName=\"custom\" textColor=\"#000000\" verticalTextPosition=\"top\" visible=\"true\" width=\"27.044921875\" x=\"2.0\" xml:space=\"preserve\" y=\"29.96875\">" ++ renderFieldNames ++ "<y:LabelModel><y:ErdAttributesNodeLabelModel/></y:LabelModel><y:ModelParameter><y:ErdAttributesNodeLabelModelParameter/></y:ModelParameter></y:NodeLabel>"
  else
    ""
  where
    renderFieldNames = L.unlines (L.map fieldName nodefields)

renderYLink :: (Int, NodeLink) -> String
renderYLink (i, nL) =
  "<edge id=\"e" ++ show i ++ "\" source=\"" ++ (fst $ sourceId nL) ++ "\" target=\"" ++ (unNodeId $ targetId nL) ++ "\">\n\
   \</edge>"

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
renderLink (NodeLink from (NodeId to)) = (fst from) ++ (M.fromMaybe "" (fmap (":" ++) (snd from))) ++ " -> " ++ to

renderGraph :: Graph -> String
renderGraph (Graph nodes links) =
  "digraph models_diagram {\n graph[overlap=false, splines=true]"
  ++ L.intercalate "\n\n" (L.map renderNode nodes)
  ++ L.intercalate "\n" (L.map renderLink links)
  ++ "}"
