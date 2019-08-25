{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Job
  ( jobClassLabels
  , Alloc(..)
  , accountJobs
  ) where

import           Control.Arrow (first)
import qualified Data.ByteString as BS
import           Data.List (foldl')
import qualified Data.Map.Strict as Map

import Slurm
import Prometheus
import TRES
import Node

data Alloc = Alloc
  { allocJob :: !Word
  , allocTRES :: !TRES
  } deriving (Show)

instance Semigroup Alloc where
  Alloc j1 t1 <> Alloc j2 t2 =
    Alloc (j1 + j2) (t1 <> t2)

instance Monoid Alloc where
  mempty = Alloc 0 mempty

jobAlloc :: JobInfo -> Alloc
jobAlloc = Alloc 1 . parseTRES . jobInfoTRES

data JobClass
  = JobPending
    { jobAccount :: !BS.ByteString
    , jobPartition :: !BS.ByteString
    , jobUser :: !BS.ByteString
    }
  | JobRunning
    { jobAccount :: !BS.ByteString
    , jobPartition :: !BS.ByteString
    , jobUser :: !BS.ByteString
    , jobNodeClass :: !BS.ByteString
    }
  | JobStopped
  deriving (Eq, Ord, Show)

jobClass :: JobInfo -> JobClass
jobClass JobInfo{..}
  | jobInfoState == jobPending = JobPending jobInfoAccount jobInfoPartition jobInfoUser
  | jobInfoState == jobRunning = JobRunning jobInfoAccount jobInfoPartition jobInfoUser
    (foldMap (nodeClass . head . expandHostList) jobInfoNodes) -- XXX only first node
  | otherwise = JobStopped

jobClassLabels :: JobClass -> Labels
jobClassLabels JobPending{..} =
  [ ("state", "pending")
  , ("account", jobAccount)
  , ("partition", jobPartition)
  , ("user", jobUser)
  ]
jobClassLabels JobRunning{..} =
  [ ("state", "running")
  , ("account", jobAccount)
  , ("partition", jobPartition)
  , ("user", jobUser)
  , ("nodes", jobNodeClass)
  ]
jobClassLabels JobStopped =
  [ ("state", "stopped")
  ]

accountJobs :: [JobInfo] -> [(Labels, Alloc)]
accountJobs = map (first jobClassLabels) . Map.toList .
  foldl' (\a j -> Map.insertWith (<>) (jobClass j) (jobAlloc j) a) Map.empty
