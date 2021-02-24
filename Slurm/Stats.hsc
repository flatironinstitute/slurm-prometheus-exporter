module Slurm.Stats
  ( slurmGetStatistics
  , StatsInfoResponseMsg(..)
  , StatsInfoUser(..)
  ) where

import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..), peek, pokeByteOff)

import Slurm.Internal
import User

#include <slurm/slurm.h>

data StatsInfoRequestMsg

data StatsInfoUser = StatsInfoUser
  { statsInfoUser :: !UserName
  , statsInfoUserCnt :: !Word32
  , statsInfoUserTime :: !Word64
  } deriving (Show)

data StatsInfoResponseMsg = StatsInfoResponse
  { statsInfoReqTime :: !CTime
  , statsInfoServerThreadCount :: !Word32
  , statsInfoJobsSubmitted
  , statsInfoJobsStarted
  , statsInfoJobsCompleted
  , statsInfoJobsCanceled
  , statsInfoJobsFailed
  , statsInfoJobsPending
  , statsInfoJobsRunning :: !Word32
  , statsInfoJobStatesTime :: Maybe CTime
  , statsInfoRpcUser :: [StatsInfoUser]
  } deriving (Show)

instance Storable StatsInfoResponseMsg where
  sizeOf _    = #size stats_info_response_msg_t
  alignment _ = #alignment stats_info_response_msg_t
  peek p = StatsInfoResponse
    <$> (#peek stats_info_response_msg_t, req_time) p
    <*> (#peek stats_info_response_msg_t, server_thread_count) p
    <*> (#peek stats_info_response_msg_t, jobs_submitted) p
    <*> (#peek stats_info_response_msg_t, jobs_started) p
    <*> (#peek stats_info_response_msg_t, jobs_completed) p
    <*> (#peek stats_info_response_msg_t, jobs_canceled) p
    <*> (#peek stats_info_response_msg_t, jobs_failed) p
#if SLURM_VERSION_NUMBER >= SLURM_VERSION_NUM(18,0,0)
    <*> (#peek stats_info_response_msg_t, jobs_pending) p
#else
    <*> return 0
#endif
    <*> (#peek stats_info_response_msg_t, jobs_running) p
#if SLURM_VERSION_NUMBER >= SLURM_VERSION_NUM(18,0,0)
    <*> (Just <$> (#peek stats_info_response_msg_t, job_states_ts) p)
#else
    <*> return Nothing
#endif
    <*> do
      nw <- (#peek stats_info_response_msg_t, rpc_user_size) p
      let n = fromIntegral (nw :: Word32)
      zipWith3 StatsInfoUser
        <$> (mapM (uidName Nothing) =<< peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_id) p)
        <*> (peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_cnt) p)
        <*> (peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_time) p)
  poke = error "poke StatsInfoResponse not implemented"

foreign import ccall unsafe slurm_get_statistics :: Ptr (Ptr StatsInfoResponseMsg) -> Ptr StatsInfoRequestMsg -> IO CInt
foreign import ccall unsafe slurm_free_stats_response_msg :: Ptr StatsInfoResponseMsg -> IO ()

slurmGetStatistics :: IO StatsInfoResponseMsg
slurmGetStatistics =
  allocaBytes (#size stats_info_request_msg_t) $ \req -> do
  alloca $ \resp -> do
    (#poke stats_info_request_msg_t, command_id) req ((#const STAT_COMMAND_GET) :: Word16)
    throwIfError $ slurm_get_statistics resp req
    res <- peek resp
    stats <- peek res
    slurm_free_stats_response_msg res
    return stats

