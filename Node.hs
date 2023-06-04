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
import           Data.Foldable (fold)
import           Data.List (foldl', find)
import qualified Data.Map.Strict as Map
import           System.Posix.Types (EpochTime)

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

nodeFromInfo :: EpochTime -> NodeInfo -> Node
nodeFromInfo now n@NodeInfo{..} = Node n
  (fold $ find (`notElem` ["local","location=local"]) $ BSC.split ',' nodeInfoFeatures)
  (parseTRES nodeInfoTRES){ tresNode = 1 }
  Alloc
    { allocTRES = (parseTRES nodeInfoTRESAlloc){ tresNode = alloc }
    , allocJob = alloc
    , allocLoad = MkFixed $ maybe 0 toInteger nodeInfoLoad
    , allocMem = 1024 * 1024 * (if nodeInfoMem > nodeInfoMemFree then nodeInfoMem - nodeInfoMemFree else 0)
    , allocTime = if nodeInfoBootTime == 0 then 0 else now - nodeInfoBootTime
    }
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
  , descReason :: !BSC.ByteString
  } deriving (Eq, Ord, Show)

type ResMap = Map.Map NodeDesc Alloc

addNode :: Bool -> Node -> ResMap -> ResMap
addNode withreason Node{..} = ar ResAlloc nodeAlloc
    { allocTime = if alloc then allocTime nodeAlloc else 0 }
  . ar (case nodeInfoState nodeInfo of
    s | s == nodeStateDrain && s /= nodeStateRebootRequested -> ResDrain
      | s == nodeStateDown -> ResDown
      | s == nodeStateRes -> ResResv
      | otherwise -> ResFree)
    mempty
      { allocTRES = nodeTRES - allocTRES nodeAlloc
      , allocTime = if alloc then 0 else allocTime nodeAlloc
      }
  where
  alloc = tresNode (allocTRES nodeAlloc) /= 0
  ar s = Map.insertWith (<>) (NodeDesc s nodeClass $
    if withreason then nodeInfoReason nodeInfo else BSC.empty)

accountNodes :: Bool -> [Node] -> [(NodeRes, Labels, Alloc)]
accountNodes withreason = map (\(n, a) ->
    ( descRes n
    , (if withreason then (("reason", descReason n) :) else id) [("nodes", descClass n)]
    , a))
  . Map.toList
  . foldl' (flip $ addNode withreason) Map.empty
