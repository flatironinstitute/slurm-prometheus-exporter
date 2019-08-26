{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Job
  ( Job(..)
  , jobFromInfo
  , JobClass(..)
  , accountJobs
  ) where

import qualified Data.ByteString as BS
import           Data.Function (on)
import           Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import Slurm
import Prometheus
import TRES
import Node

data Job = Job
  { jobInfo :: !JobInfo
  , jobNodes :: [Node]
  , jobAlloc :: Alloc
  } deriving (Show)

jobFromInfo :: NodeMap -> JobInfo -> Job
jobFromInfo nm j@JobInfo{..} = Job j nodes
  $ nalloc
    { allocTRES = tres
    , allocJob = 1
    }
  where
  tres = parseTRES jobInfoTRES
  nodes = map (\n -> fromMaybe (nodeFromName n) $ Map.lookup n nm) $
    foldMap expandHostList jobInfoNodes
  nalloc = case map nodeAlloc nodes of
    -- scale used load and memory by fraction of allocation (assumes only shared single-nodes)
    [alloc] | f < 1 -> alloc
      { allocLoad = realToFrac $ f * realToFrac (allocLoad alloc)
      , allocMem  = round      $ f * realToFrac (allocMem alloc)
      } where f = realToFrac (tresCPU tres) / realToFrac (tresCPU (allocTRES alloc)) :: Float
    allocs -> sum allocs

data JobClass
  = JobRunning
  | JobPending
  deriving (Eq, Ord, Show)

instance Labeled JobClass where
  label JobRunning = "running"
  label JobPending = "pending"

data JobDesc = JobDesc
  { jobClass :: !JobClass
  , jobAccount :: !BS.ByteString
  , jobPartition :: !BS.ByteString
  , jobUser :: !BS.ByteString
  , jobNodeClass :: !BS.ByteString
  } deriving (Eq, Ord, Show)

-- |Like nub but with counts.
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:l) = (x,c) : rle r where
  (c, r) = countx 1 l
  countx n (y:z) | y == x = countx (succ n) z
  countx n z = (n, z)

jobDesc :: Job -> Maybe JobDesc
jobDesc Job{ jobInfo = JobInfo{..}, .. } = do
  c <- case jobInfoState of
    s | s == jobRunning -> Just JobRunning
      | s == jobPending -> Just JobPending
      | otherwise -> Nothing
  return $ JobDesc c jobInfoAccount jobInfoPartition jobInfoUser
    $ if null jobNodes then mempty else fst $ maximumBy (compare `on` snd) $ rle $ map nodeClass jobNodes

jobLabels :: JobDesc -> Labels
jobLabels JobDesc{..} =
  -- ("state", jobClassStr jobClass)
  [ ("account", jobAccount)
  , ("partition", jobPartition)
  , ("user", jobUser)
  ] ++ 
  if BS.null jobNodeClass then [] else
  [ ("nodes", jobNodeClass)
  ]

accountJobs :: [Job] -> [(JobClass, Labels, Alloc)]
accountJobs = map (\(d, a) -> (jobClass d, jobLabels d, a))
  . Map.toList
  . foldl' (\a j -> maybe id
      (\c -> Map.insertWith (<>) c (jobAlloc j))
      (jobDesc j) a)
    Map.empty
