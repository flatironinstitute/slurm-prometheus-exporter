{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Slurm.Job
  ( JobState
  , jobPending
  , jobRunning
  , jobSuspended
  , jobComplete
  , jobCancelled
  , jobFailed
  , jobTimeout
  , jobNodeFail
  , jobPreempted
  , jobBootFail
  , jobDeadline
  , jobOOM
  , JobInfo(..)
  , slurmLoadJobs
  ) where

import           Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.ByteString as BS
import           Data.Bits ((.&.), popCount)
import           Data.Word (Word16, Word32)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr)
import           Foreign.Storable (Storable(..), peek)
import qualified System.IO.Unsafe as Unsafe

import Slurm.Internal
import User

#include <slurm/slurm.h>

newtype JobState = JobState Word32
  deriving (Storable, Show)
#enum JobState, JobState, \
  JOB_PENDING, \
  JOB_RUNNING, \
  JOB_SUSPENDED,\
  JOB_COMPLETE,	\
  JOB_CANCELLED,\
  JOB_FAILED,	\
  JOB_TIMEOUT,	\
  JOB_NODE_FAIL,\
  JOB_PREEMPTED,\
  JOB_BOOT_FAIL,\
  JOB_DEADLINE,	\
  jobOOM = JOB_OOM

-- sort of a hacky, assymetric, broken "equals"
instance Eq JobState where
  JobState s == JobState t
    | f == 0 = (s .&. #const JOB_STATE_BASE) == t
    | popCount f == 1 = s .&. t /= 0
    | otherwise = s == t
    where f = t .&. #const JOB_STATE_FLAGS

data JobInfo = JobInfo
  { jobInfoId :: !Word32
  , jobInfoState :: !JobState
  , jobInfoNodes :: Maybe NodeName
  , jobInfoAccount
  , jobInfoPartition :: !BS.ByteString
  , jobInfoUser :: !UserName
  , jobInfoSubmitTime
  , jobInfoStartTime
  , jobInfoEndTime :: !CTime
  , jobInfoTRES :: !BS.ByteString
  } deriving (Show)

instance Storable JobInfo where
  sizeOf _    = #size slurm_job_info_t
  alignment _ = #alignment slurm_job_info_t
  peek p = JobInfo
    <$> (#peek slurm_job_info_t, job_id) p
    <*> (#peek slurm_job_info_t, job_state) p
    <*> (maybeCString =<< (#peek slurm_job_info_t, nodes) p)
    <*> (packCString =<< (#peek slurm_job_info_t, account) p)
    <*> (packCString =<< (#peek slurm_job_info_t, partition) p)
    <*> (maybe (uidName =<< (#peek slurm_job_info_t, user_id) p) return =<< maybeCString
       =<< (#peek slurm_job_info_t, user_name) p)
    <*> (#peek slurm_job_info_t, submit_time) p
    <*> (#peek slurm_job_info_t, start_time) p
    <*> (#peek slurm_job_info_t, end_time) p
    <*> (packCString
        =<< maybe ((#peek slurm_job_info_t, tres_req_str) p) return . maybePtr
        =<< (#peek slurm_job_info_t, tres_alloc_str) p)
  poke = error "poke JobInfo not implemented"

data JobInfoMsg

peekJobs :: Ptr JobInfoMsg -> IO (CTime, [JobInfo])
peekJobs p = do
  t <- (#peek job_info_msg_t, last_update) p
  n <- (#peek job_info_msg_t, record_count) p
  (t, ) <$> (peekArray n =<< (#peek job_info_msg_t, job_array) p)

foreign import ccall unsafe slurm_load_jobs :: CTime -> Ptr (Ptr JobInfoMsg) -> Word16 -> IO CInt
foreign import ccall unsafe "&slurm_free_job_info_msg" slurm_free_job_info_msg_ptr :: FunPtr (Ptr JobInfoMsg -> IO ())

{-# NOINLINE jobInfo #-}
jobInfo :: MVar (ForeignPtr JobInfoMsg)
jobInfo = Unsafe.unsafePerformIO $ newMVar =<< newForeignPtr_ nullPtr

slurmLoadJobs :: IO (CTime, [JobInfo])
slurmLoadJobs = loadWhenChanged jobInfo
  (\p r -> slurm_load_jobs p r (#const SHOW_ALL))
  slurm_free_job_info_msg_ptr
  (#peek job_info_msg_t, last_update)
  peekJobs
