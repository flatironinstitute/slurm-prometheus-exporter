{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Node
  ( nodeFromInfo
  , nodeFromName
  , Node(..)
  , NodeMap
  , nodeMap
  , accountNodes
  ) where

import           Control.Arrow (first, (&&&))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isAlpha, isDigit)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Slurm
import TRES
import Prometheus

-- |Reduce a node name to some kind of classification. Specific to FI cluster config.
nodeNameClass :: NodeName -> NodeName 
nodeNameClass n
  | not (BS.null a) = a
  | not (BS.null d) = BS.take 1 d <> BSC.replicate (pred $ BS.length d) '0'
  | otherwise = mempty
  where
  a = BSC.takeWhile isAlpha s
  d = BSC.takeWhile isDigit s
  s = fromMaybe n $ BS.stripPrefix "worker" n

data Node = Node
  { nodeInfo :: !NodeInfo
  , nodeClass :: !NodeName
  }

nodeFromName :: NodeName -> Node
nodeFromName n = Node unknownNodeInfo{ nodeInfoName = n } (nodeNameClass n)

nodeFromInfo :: NodeInfo -> Node
nodeFromInfo n = Node n (nodeNameClass $ nodeInfoName n)

type NodeMap = Map.Map NodeName Node

nodeMap :: [Node] -> NodeMap
nodeMap = Map.fromList . map (nodeInfoName . nodeInfo &&& id)

data ResState
  = ResAlloc
  | ResFree
  | ResDrain
  | ResResv
  | ResDown
  deriving (Eq, Ord, Enum, Show)

data NodeRes = NodeRes
  { resNode :: !NodeName
  , resState :: !ResState
  } deriving (Eq, Ord, Show)

nodeResources :: Node -> [(NodeRes, TRES)]
nodeResources Node{ nodeInfo = NodeInfo{..}, ..} = [n ResAlloc ares, n s (tres - ares)]
  where
  s | nodeInfoState == nodeStateDrain && nodeInfoState /= nodeStateReboot = ResDrain
    | nodeInfoState == nodeStateDown = ResDown
    | nodeInfoState == nodeStateRes = ResResv
    | otherwise = ResFree
  n = (,) . NodeRes nodeClass
  tres = (parseTRES nodeInfoTRES){ tresNode = 1 }
  ares = (parseTRES nodeInfoTRESAlloc){ tresNode = if nodeInfoState == nodeStateAllocated then 1 else 0 }

resLabels :: NodeRes -> Labels
resLabels NodeRes{..} =
  [ ("nodes", resNode)
  , ("state", case resState of
      ResAlloc -> "alloc"
      ResFree -> "free"
      ResDrain -> "drain"
      ResResv -> "resv"
      ResDown -> "down")
  ]
  
accountNodes :: [Node] -> [(Labels, TRES)]
accountNodes = map (first resLabels) . Map.toList .
  Map.fromListWith (<>) . foldMap nodeResources
