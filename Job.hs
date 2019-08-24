{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Job
  ( jobClassLabels
  , Alloc(..)
  , accountJobs
  ) where

import           Control.Arrow (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isAlpha, isDigit)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16, Word32, Word64)

import Slurm
import Prometheus

data Alloc = Alloc
  { allocJob :: !Word
  , allocNode :: !Word16
  , allocCPU :: !Word32
  , allocMem :: !Word64
  , allocGPU :: !Word16
  } deriving (Show)

instance Semigroup Alloc where
  Alloc j1 n1 c1 m1 g1 <> Alloc j2 n2 c2 m2 g2 =
    Alloc (j1 + j2) (n1 + n2) (c1 + c2) (m1 + m2) (g1 + g2)

instance Monoid Alloc where
  mempty = Alloc 0 0 0 0 0

parseTRES :: BS.ByteString -> Alloc
parseTRES s = foldMap br $ BSC.split ',' s where
  br (BSC.break ('=' ==) -> (k, BSC.uncons -> Just ('=', v))) = pt k v
  br _ = mempty
  pt "node" (pn -> Just n) = mempty{ allocNode = fromInteger n }
  pt "cpu" (pn -> Just n) = mempty{ allocCPU = fromInteger n }
  pt "mem" (pn -> Just n) = mempty{ allocMem = fromInteger n }
  pt "gres/gpu" (pn -> Just n) = mempty{ allocGPU = fromInteger n }
  pt _ _ = mempty
  pn (BSC.readInteger -> Just (n, flip lookup units -> Just u)) = Just (u * n)
  pn _ = Nothing
  units = zip ["","K","M","G","T","P"] $ iterate (1024*) 1

jobAlloc :: JobInfo -> Alloc
jobAlloc JobInfo{..} = (parseTRES jobInfoTRES){ allocJob = 1 }

-- |Reduce a node name to some kind of classification. Specific to FI cluster config.
nodeClass :: NodeName -> NodeName 
nodeClass n
  | not (BS.null a) = a
  | not (BS.null d) = BS.take 1 d <> BSC.replicate (pred $ BS.length d) '0'
  | otherwise = mempty
  where
  a = BSC.takeWhile isAlpha s
  d = BSC.takeWhile isDigit s
  s = fromMaybe n $ BS.stripPrefix "worker" n

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
  | JobState
    { jobState :: !BS.ByteString
    }
  deriving (Eq, Ord, Show)

jobClass :: JobInfo -> JobClass
jobClass JobInfo{..}
  | jobInfoState == jobPending = JobPending jobInfoAccount jobInfoPartition jobInfoUser
  | jobInfoState == jobRunning = JobRunning jobInfoAccount jobInfoPartition jobInfoUser
    (foldMap (nodeClass . head . expandHostList) jobInfoNodes) -- XXX only first node
  | jobInfoState == jobSuspended = JobState "suspend"
  | jobInfoState == jobComplete  = JobState "complete"
  | jobInfoState == jobCancelled = JobState "cancel"
  | jobInfoState == jobFailed    = JobState "fail"
  | jobInfoState == jobTimeout   = JobState "timeout"
  | jobInfoState == jobNodeFail  = JobState "nodefail"
  | jobInfoState == jobPreempted = JobState "preemt"
  | jobInfoState == jobBootFail  = JobState "nodefail"
  | jobInfoState == jobDeadline  = JobState "timeout"
  | jobInfoState == jobOOM       = JobState "fail"
  | otherwise                    = JobState "unknown"

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
jobClassLabels JobState{..} =
  [ ("state", jobState)
  ]

accountJobs :: [JobInfo] -> [(Labels, Alloc)]
accountJobs = map (first jobClassLabels) . Map.toList .
  foldl' (\a j -> Map.insertWith (<>) (jobClass j) (jobAlloc j) a) Map.empty
