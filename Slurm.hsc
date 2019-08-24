{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Slurm
  ( NodeName
  , expandHostList'
  , expandHostList
  , slurmGetStatistics
  , StatsInfoResponseMsg(..)
  , StatsInfoUser(..)
  , JobState
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

import           Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar)
import           Control.Exception (bracket)
import           Control.Monad ((<=<), mfilter)
import qualified Data.ByteString as BS
import           Data.Foldable (fold)
import           Data.Function (fix)
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.String (CString, peekCString)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr)
import           Foreign.Storable (Storable(..), peek, pokeByteOff)
import           System.IO.Error (ioError, userError)
import qualified System.IO.Unsafe as Unsafe

import User

#include <slurm/slurm.h>

#let alloca type = "allocaBytes (%lu)", sizeof(type)

maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr = mfilter (/= nullPtr) . Just

maybeCString :: CString -> IO (Maybe BS.ByteString)
maybeCString = mapM BS.packCString . maybePtr

packCString :: CString -> IO BS.ByteString
packCString = fmap fold . maybeCString

foreign import ccall unsafe slurm_strerror :: CInt -> CString
foreign import ccall unsafe slurm_get_errno :: IO CInt

{-# NOINLINE errnoLock #-}
errnoLock :: MVar ()
errnoLock = Unsafe.unsafePerformIO $ newMVar ()

checkError :: IO CInt -> IO (Maybe CInt)
checkError f = withMVar errnoLock $ \() -> do
  e <- f
  if e == (#const SLURM_SUCCESS) then return Nothing else Just <$> slurm_get_errno

throwError :: CInt -> IO ()
throwError = ioError . userError <=< peekCString . slurm_strerror

throwIfError :: IO CInt -> IO ()
throwIfError = mapM_ throwError <=< checkError

type NodeName = BS.ByteString

data HostList

foreign import ccall unsafe slurm_hostlist_create :: CString -> IO (Ptr HostList)
foreign import ccall unsafe slurm_hostlist_destroy :: Ptr HostList -> IO ()
foreign import ccall unsafe "&slurm_hostlist_destroy" slurm_hostlist_destroy_ptr :: FunPtr (Ptr HostList -> IO ())
foreign import ccall unsafe slurm_hostlist_shift :: Ptr HostList -> IO CString

hostlistShift :: Ptr HostList -> IO (Maybe BS.ByteString)
hostlistShift h = bracket
  (slurm_hostlist_shift h)
  free
  maybeCString

getList :: IO (Maybe a) -> IO [a] -> IO [a]
getList h r = h >>= maybe
  (return [])
  (\n -> (n :) <$> r)

expandHostList' :: NodeName -> IO [NodeName]
expandHostList' s = bracket
  (BS.useAsCString s slurm_hostlist_create)
  slurm_hostlist_destroy
  $ fix . getList . hostlistShift

expandHostList :: NodeName -> [NodeName]
expandHostList s = Unsafe.unsafeDupablePerformIO $ do
  h <- newForeignPtr slurm_hostlist_destroy_ptr =<< BS.useAsCString s slurm_hostlist_create
  fix $ getList (withForeignPtr h hostlistShift) . Unsafe.unsafeInterleaveIO

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
        <$> (mapM uidName =<< peekArray n =<< (#peek stats_info_response_msg_t, rpc_user_id) p)
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
    throwIfError $ slurm_get_statistics resp req
    res <- peek resp
    stats <- peek res
    slurm_free_stats_response_msg res
    return stats

data JobInfoMsg

newtype JobState = JobState Word32
  deriving (Eq, Enum, Storable, Show)
#enum JobState, JobState, \
  JOB_PENDING, \
  JOB_RUNNING, \
  JOB_SUSPENDED, \
  JOB_COMPLETE, \
  JOB_CANCELLED, \
  JOB_FAILED, \
  JOB_TIMEOUT, \
  JOB_NODE_FAIL, \
  JOB_PREEMPTED, \
  JOB_BOOT_FAIL, \
  JOB_DEADLINE,	 \
  jobOOM = JOB_OOM

data JobInfo = JobInfo
  { jobInfoAccount :: !BS.ByteString
  , jobInfoId :: !Word32
  , jobInfoState :: !JobState
  , jobInfoNodes :: Maybe NodeName
  , jobInfoPartition :: !BS.ByteString
  , jobInfoUser :: !UserName
  , jobInfoTRES :: !BS.ByteString
  } deriving (Show)

instance Storable JobInfo where
  sizeOf _    = #size slurm_job_info_t
  alignment _ = #alignment slurm_job_info_t
  peek p = JobInfo
    <$> (packCString =<< (#peek slurm_job_info_t, account) p)
    <*> (#peek slurm_job_info_t, job_id) p
    <*> (#peek slurm_job_info_t, job_state) p
    <*> (maybeCString =<< (#peek slurm_job_info_t, nodes) p)
    <*> (packCString =<< (#peek slurm_job_info_t, partition) p)
    <*> (maybe (uidName =<< (#peek slurm_job_info_t, user_id) p) return =<< maybeCString
       =<< (#peek slurm_job_info_t, user_name) p)
    <*> (packCString
        =<< maybe ((#peek slurm_job_info_t, tres_req_str) p) return . maybePtr
        =<< (#peek slurm_job_info_t, tres_alloc_str) p)
  poke = error "poke JobInfo not implemented"

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
slurmLoadJobs = modifyMVar jobInfo $ \info ->
  alloca $ \resp -> do
    prev <- withForeignPtr info $ \infop ->
      maybe (return 0) (#peek job_info_msg_t, last_update) $ maybePtr infop
    err <- checkError $ slurm_load_jobs prev resp (#const SHOW_ALL)
    info' <- maybe
      (newForeignPtr slurm_free_job_info_msg_ptr =<< peek resp)
      (\e -> info <$ if e == (#const SLURM_NO_CHANGE_IN_DATA)
        then return ()
        else throwError e)
      err
    (info', ) <$> withForeignPtr info' peekJobs
