{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Node
  ( nodeFromInfo
  , nodeFromName
  , Node(..)
  , NodeMap
  , nodeMap
  , NodeRes(..)
  , accountNodes
  ) where

import           Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as BSC
import           Data.Fixed (Fixed(..))
import           Data.List (foldl')
import qualified Data.Map.Strict as Map

import Slurm
import TRES
import Prometheus

data Node = Node
  { nodeInfo :: !NodeInfo
  , nodeClass :: NodeName
  , nodeTRES :: TRES 
  , nodeAlloc :: Alloc
  } deriving (Show)

nodeFromName :: NodeName -> Node
nodeFromName n = Node unknownNodeInfo{ nodeInfoName = n } mempty mempty mempty

nodeFromInfo :: NodeInfo -> Node
nodeFromInfo n@NodeInfo{..} = Node n
  (BSC.takeWhile (',' /=) nodeInfoFeatures)
  (parseTRES nodeInfoTRES){ tresNode = 1 }
  (Alloc (parseTRES nodeInfoTRESAlloc){ tresNode = alloc } alloc
    (MkFixed $ maybe 0 toInteger nodeInfoLoad)
    (1024 * 1024 * (nodeInfoMem - nodeInfoMemFree)))
  where
  alloc :: Integral i => i
  alloc = if nodeInfoState == nodeStateAllocated then 1 else 0

type NodeMap = Map.Map NodeName Node

nodeMap :: [Node] -> NodeMap
nodeMap = Map.fromList . map (nodeInfoName . nodeInfo &&& id)

data NodeRes
  = ResAlloc
  | ResFree
  | ResDrain
  | ResResv
  | ResDown
  deriving (Eq, Ord, Show)

instance Labeled NodeRes where
  label ResAlloc = "alloc"
  label ResFree = "free"
  label ResDrain = "drain"
  label ResResv = "resv"
  label ResDown = "down"

data NodeDesc = NodeDesc
  { descRes :: !NodeRes
  , descClass :: !NodeName
  } deriving (Eq, Ord, Show)

type ResMap = Map.Map NodeDesc Alloc

addNode :: Node -> ResMap -> ResMap
addNode Node{..} = ar ResAlloc nodeAlloc
  . ar (case nodeInfoState nodeInfo of
    s | s == nodeStateDrain && s /= nodeStateReboot -> ResDrain
      | s == nodeStateDown -> ResDown
      | s == nodeStateRes -> ResResv
      | otherwise -> ResFree)
    mempty{ allocTRES = nodeTRES - allocTRES nodeAlloc }
  where
  ar s = Map.insertWith (<>) (NodeDesc s nodeClass)

accountNodes :: [Node] -> [(NodeRes, Labels, Alloc)]
accountNodes = map (\(n, a) -> (descRes n, [("nodes", descClass n)], a))
  . Map.toList
  . foldl' (flip addNode) Map.empty
