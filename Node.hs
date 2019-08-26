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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isAlpha, isDigit)
import           Data.Fixed (Fixed(..))
import           Data.List (foldl')
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
  , nodeClass :: NodeName
  , nodeTRES :: TRES 
  , nodeAlloc :: Alloc
  }

nodeFromName :: NodeName -> Node
nodeFromName n = Node unknownNodeInfo{ nodeInfoName = n } (nodeNameClass n) mempty mempty

nodeFromInfo :: NodeInfo -> Node
nodeFromInfo n@NodeInfo{..} = Node n (nodeNameClass nodeInfoName)
  (parseTRES nodeInfoTRES){ tresNode = 1 }
  (Alloc (parseTRES nodeInfoTRESAlloc){ tresNode = alloc } alloc
    (MkFixed $ maybe 0 toInteger nodeInfoLoad)
    (nodeInfoMem - nodeInfoMemFree))
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
