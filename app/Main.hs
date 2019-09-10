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
  { hello      :: Maybe String
  , noFields :: Bool
  , onlyLinkFields :: Bool
  , outFile      :: Maybe String
  , filePath      :: [String] }

sample :: Parser Sample
sample = Sample
  <$> optional (
    strOption
    ( long "hello"
      <> metavar "TARGET"
      <> help "Target for the greeting" ))
  <*> switch
      ( long "show-fields"
      <> short 'f'
      <> help "Whether to include datafields at all" )
  <*> switch
      ( long "only-link-fields"
      <> short 'l'
      <> help "Whether to show only Links" )
  <*> optional (strOption
          ( long "out"
         <> metavar "TARGET"
         <> short 'o'
         <> (help $ "specify the output filename ")))
      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "How enthusiastically to greet"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )
      <*> many (argument str (metavar "FILEPATH"))


main :: IO ()
main = do
  args <- execParser opts
  putStrLn $ "Generating ER for all doctypes found beneath"
  mapM putStrLn (filePath args)
  renderDocTypes (onlyLinkFields args) (fromMaybe "out.dot" $ outFile args) $ filePath args
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc ""
     <> header "Generating ER-Diagrams for frappe Doctypes" )

greet :: Sample -> IO ()
greet _ = return ()




-- main :: IO ()
-- main = do
--   currentDir <- listDirectory "/home/steffen/Bilder"
--   _ <- mapM putStrLn currentDir

