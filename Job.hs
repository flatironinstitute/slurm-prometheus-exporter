{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Job
  ( Job(..)
  , jobFromInfo
  , jobClassLabels
  , Alloc(..)
  , accountJobs
  ) where

import           Control.Arrow (first)
import qualified Data.ByteString as BS
import           Data.Fixed (Fixed(..), Centi)
import           Data.Function (on)
import           Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import Slurm
import Prometheus
import TRES
import Node

data Job = Job
  { jobInfo :: JobInfo
  , jobNodes :: [Node]
  }

jobFromInfo :: NodeMap -> JobInfo -> Job
jobFromInfo nm j = Job j
  $ map (\n -> fromMaybe (nodeFromName n) $ Map.lookup n nm)
  $ foldMap expandHostList $ jobInfoNodes j

data Alloc = Alloc
  { allocJob :: !Word
  , allocTRES :: !TRES
  , allocLoad :: !Centi
  } deriving (Show)

instance Semigroup Alloc where
  Alloc j1 t1 l1 <> Alloc j2 t2 l2 =
    Alloc (j1 + j2) (t1 <> t2) (l1 + l2)

instance Monoid Alloc where
  mempty = Alloc 0 mempty 0

jobAlloc :: Job -> Alloc
jobAlloc j = Alloc 1 (parseTRES $ jobInfoTRES $ jobInfo j)
  $ sum $ map (maybe 0 (MkFixed . fromIntegral) . nodeInfoLoad . nodeInfo) (jobNodes j)

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
  deriving (Eq, Ord, Show)

-- |Like nub but with counts.
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:l) = (x,succ c) : rle r where
  (c, r) = countx 0 l
  countx n (y:z) | y == x = countx (succ n) z
  countx n z = (n, z)

jobClass :: Job -> Maybe JobClass
jobClass Job{ jobInfo = JobInfo{..}, .. }
  | jobInfoState == jobPending = Just $ JobPending jobInfoAccount jobInfoPartition jobInfoUser
  | jobInfoState == jobRunning = Just $ JobRunning jobInfoAccount jobInfoPartition jobInfoUser
    $ if null jobNodes then mempty else fst $ maximumBy (compare `on` snd) $ rle $ map nodeClass jobNodes
  | otherwise = Nothing

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

accountJobs :: [Job] -> [(Labels, Alloc)]
accountJobs = map (first jobClassLabels) . Map.toList .
  foldl' (\a j -> maybe id
      (\c -> Map.insertWith (<>) c (jobAlloc j))
      (jobClass j) a)
    Map.empty
