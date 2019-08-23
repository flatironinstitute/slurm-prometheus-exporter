module Slurm
  ( slurmGetStatistics
  , StatsInfoResponseMsg(..)
  , StatsInfoUser(..)
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Monad (unless)
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.String (CString, peekCString)
import           Foreign.C.Types (CInt(..), CTime)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..), peek, pokeByteOff)
import           System.IO.Error (ioError, userError)
import qualified System.IO.Unsafe as Unsafe

#include <slurm.h>

#let alloca type = "allocaBytes (%lu)", sizeof(type)

foreign import ccall unsafe slurm_strerror :: CInt -> CString
foreign import ccall unsafe slurm_get_errno :: IO CInt

{-# NOINLINE errnoLock #-}
errnoLock :: MVar ()
errnoLock = Unsafe.unsafePerformIO $ newMVar ()

checkError :: IO CInt -> IO ()
checkError f = withMVar errnoLock $ \() -> do
  e <- f
  unless (e == #const SLURM_SUCCESS) $
    ioError . userError =<< peekCString . slurm_strerror =<< slurm_get_errno

data StatsInfoRequestMsg

data StatsInfoUser = StatsInfoUser
  { statsInfoUserId :: !Word32
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
  , statsInfoJobStatesTime :: !CTime
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
    <*> (#peek stats_info_response_msg_t, jobs_pending) p
    <*> (#peek stats_info_response_msg_t, jobs_running) p
    <*> (#peek stats_info_response_msg_t, job_states_ts) p
    <*> do
      n <- (#peek stats_info_response_msg_t, rpc_user_size) p
      zipWith3 StatsInfoUser
        <$> (peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_id) p)
        <*> (peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_cnt) p)
        <*> (peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_time) p)
  poke = error "poke StatsInfoResponse not implemented"

foreign import ccall unsafe slurm_get_statistics :: Ptr (Ptr StatsInfoResponseMsg) -> Ptr StatsInfoRequestMsg -> IO CInt
foreign import ccall unsafe slurm_free_stats_response_msg :: Ptr StatsInfoResponseMsg -> IO ()

slurmGetStatistics :: IO StatsInfoResponseMsg
slurmGetStatistics =
  (#alloca stats_info_request_msg_t) $ \req -> do
  alloca $ \resp -> do
    (#poke stats_info_request_msg_t, command_id) req ((#const STAT_COMMAND_GET) :: Word16)
    checkError $ slurm_get_statistics resp req
    res <- peek resp
    stats <- peek res
    slurm_free_stats_response_msg res
    return stats
