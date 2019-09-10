{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module DocType.DocGraph
  (renderDocTypes ) where

import           Control.Monad
import           Data.Aeson           as JSON
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.UTF8 as BSU
import           Data.List            as L
import           Data.Maybe           as M
import           Data.Text            as T
import           DocType.DocType
import           GHC.Generics
import           Graph.Graph          as G
import           System.Directory

docListToGraph :: Bool -> [DocType] -> Graph
docListToGraph includeFields list =
  let (nodes, links) = docListToNodes includeFields list
  in G.mkGraph nodes $ L.concat links

crosstable :: DocType -> Bool
crosstable x = linkAmount == 2 && fieldAmount <= 1
  where linkAmount = L.length $ L.filter (\x -> fieldtype x == "Link") $ fields x
        fieldAmount = L.length $ L.filter (\x -> fieldtype x /= "Link") $ fields x

docListToNodes :: Bool -> [DocType] -> ([Node], [[NodeLink]])
docListToNodes includeFields docs =
  let nodeId x = mkNodeId $ name x
      nodeColor x =
        case crosstable x of
          True  -> "gray"
          False -> "blue"
  in unzip $ L.map (\x -> (mkNode (name x) (nodeId x) (genRelevantNodeFields $ fields x) (nodeColor x),
                           getLinks (nodeId x) $ fields x)) docs
  where genRelevantNodeFields fields=
          L.map mkNodeFields
          $ L.filter filterFields
          fields
        filterFields f =
           if includeFields
           then not $ (fieldname f) `elem` ["Section Break", "Column Break"]
           else (fieldtype f) `L.elem` ["Link", "Table"]

        mkNodeFields :: DocField -> G.NodeField
        mkNodeFields doc =
          let fieldName = fieldname doc
              fieldType = fieldtype doc
              fieldOptions = if isLink doc then options doc else Nothing
          in G.mkNodeField fieldName fieldName fieldType fieldOptions

isLink :: DocField -> Bool
isLink doc = fieldtype doc `L.elem` ["Link", "Table"]

getLinks :: NodeId -> [DocField] -> [NodeLink]
getLinks nodeId fields =
  L.concat $
  L.map (\field ->
           M.maybeToList $ mkNodeLink (unNodeId nodeId ++ ":" ++ fieldname field) . mkNodeId <$> options field) $
  L.filter isLink fields

getSubDirs:: FilePath -> IO [FilePath]
getSubDirs fp =
  (Prelude.map (fp ++) <$> listDirectory fp)
  >>= (Control.Monad.filterM doesDirectoryExist)

findDocTypes :: FilePath -> IO [FilePath]
findDocTypes fp =
  do
    jsonsHere <- L.filter isJson <$> L.map mkFullPath <$> listDirectory fp
    subDirs <- L.map (++ "/") <$> getSubDirs fp
    deeperJsons <- L.concat <$> mapM findDocTypes subDirs
    return $ jsonsHere ++ deeperJsons
  where
    mkFullPath p = fp ++ "/" ++ p
    isJson fp = andWith [L.isSuffixOf ".json", L.isInfixOf "doctype"] fp
    andWith :: [a->Bool] -> a -> Bool
    andWith fs elem = and $ L.map ($ elem) fs

getAllDoctypes :: FilePath -> IO [Either String DocType]
getAllDoctypes fp = findDocTypes fp >>= mapM getDocTypeFromJson

renderDocTypes :: Bool -> FilePath -> [FilePath] -> IO ()
renderDocTypes includeFields outF fp =
  do
    docTypes <- mapM getAllDoctypes fp
    writeFile outF $ renderGraph $ docListToGraph includeFields $ M.mapMaybe (\x -> case x of
                                                                    Left _ -> Nothing
                                                                    Right a -> Just a) $ L.concat docTypes
