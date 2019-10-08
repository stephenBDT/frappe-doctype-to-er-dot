{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import Data.Text as T
import Data.Maybe as M
import DocType.DocGraph
import Graph.Graph as G
import DocType.DocType
import Data.List as L
import Data.Aeson as JSON
import System.Directory
import GHC.Generics
import Control.Monad
import Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { onlyLinkFields :: Bool
  , yedFormat :: Bool
  , outFile      :: Maybe String
  , filePath      :: [String] }

sample :: Parser Sample
sample = Sample
  <$>  switch
      ( long "only-link-fields"
      <> short 'l'
      <> help "Whether to show only Links" )
  <*> switch
      ( long "output-graphml"
      <> short 'y'
      <> help "Whether to output .graphml file" )
  <*> optional (strOption
          ( long "out"
         <> metavar "TARGET"
         <> short 'o'
         <> (help $ "specify the output filename ")))
      <*> many (argument str (metavar "FILEPATH"))


main :: IO ()
main = do
  args <- execParser opts
  putStrLn $ "Generating ER for all doctypes found beneath"
  mapM putStrLn (filePath args)
  renderDocTypes (format args) (not $ onlyLinkFields args) (fromMaybe (defaultOutputName args) $ outFile args) $ filePath args
  where
    defaultOutputName args = 
      case yedFormat args of
        True -> "out.graphml"
        False -> "out.dot"
    format args =
      case yedFormat args of
        True -> GraphML
        False -> Dot
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc ""
     <> header "Generating ER-Diagrams for frappe Doctypes" )

greet :: Sample -> IO ()
greet _ = return ()
