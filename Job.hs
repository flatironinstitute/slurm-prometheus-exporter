{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Job
  ( Job(..)
  , jobFromInfo
  , JobClass(..)
  , accountJobs
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Function (on)
import           Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word32)
import           System.Posix.Types (EpochTime)

import Slurm
import Prometheus
import TRES
import Node

data JobClass
  = JobRunning
  | JobPending
  deriving (Eq, Ord, Show)

data Job = Job
  { jobInfo :: !JobInfo
  , jobState :: Maybe JobClass
  , jobNodes :: [Node]
  , jobAlloc :: Alloc
  } deriving (Show)

jobFromInfo :: EpochTime -> NodeMap -> JobInfo -> Job
jobFromInfo now nm j@JobInfo{..} = Job j state nodes
  nalloc
    { allocTRES = tres
    , allocJob = 1
    , allocTime = case state of
        Nothing -> jobInfoEndTime - jobInfoStartTime
        Just JobPending -> now - jobInfoSubmitTime
        Just JobRunning -> now - jobInfoStartTime
    }
  where
  state = case jobInfoState of
    s | s == jobRunning -> Just JobRunning
      | s == jobPending -> Just JobPending
      | otherwise -> Nothing
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

instance Labeled JobClass where
  label JobRunning = "running"
  label JobPending = "pending"

data JobDesc = JobDesc
  { jobClass :: !JobClass
  , jobAccount :: !BS.ByteString
  , jobPartition :: !BS.ByteString
  , jobUser :: !BS.ByteString
  , jobId :: !Word32
  , jobNodeClass :: !BS.ByteString
  } deriving (Eq, Ord, Show)

-- |Like nub but with counts.
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:l) = (x,c) : rle r where
  (c, r) = countx 1 l
  countx n (y:z) | y == x = countx (succ n) z
  countx n z = (n, z)

jobDesc :: Options -> Job -> Maybe JobDesc
jobDesc opts Job{ jobInfo = JobInfo{..}, .. } = do
  c <- jobState
  return $ JobDesc c jobInfoAccount jobInfoPartition jobInfoUser
    (if optJobId opts then jobInfoId else 0)
    $ if null jobNodes then mempty else fst $ maximumBy (compare `on` snd) $ rle $ map nodeClass jobNodes

jobLabels :: Options -> JobDesc -> Labels
jobLabels opts JobDesc{..} =
  -- ("state", jobClassStr jobClass)
  [ ("account", jobAccount)
  , ("partition", jobPartition)
  , ("user", jobUser)
  ] ++ 
  (if optJobId opts then
  [ ("jobid", BSL.toStrict $ BSB.toLazyByteString $ BSB.word32Dec jobId) ] else []) ++
  (if jobClass == JobPending then [] else
  [ ("nodes", jobNodeClass) ])

accountJobs :: Options -> [Job] -> [(JobClass, Labels, Alloc)]
accountJobs opts = map (\(d, a) -> (jobClass d, jobLabels opts d, a))
  . Map.toList
  . foldl' (\a j -> maybe id
      (\c -> Map.insertWith (<>) c (jobAlloc j))
      (jobDesc opts j) a)
    Map.empty
