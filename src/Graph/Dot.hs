{-# LANGUAGE OverloadedStrings #-}

module Dot
  (
  ) where

data Node =
  Node { nodeName :: String
       , nodeFields :: [NodeField]
       }

data NodeField =
  NodeField { fieldName :: String
            , fieldType :: String
            , fieldLink :: Maybe String
            }

  
