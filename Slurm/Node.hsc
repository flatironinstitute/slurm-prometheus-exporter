{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Slurm.Node
  ( NodeState
  , nodeStateDown
  , nodeStateIdle
  , nodeStateAllocated
  , nodeStateMixed
  , nodeStateDrain
  , nodeStateMaint
  , nodeStateRebootRequested
  , nodeStateRes
  , NodeInfo(..)
  , unknownNodeInfo
  , slurmLoadNodes
  ) where

import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Monad (mfilter)
import           Data.Bits ((.&.), popCount)
import qualified Data.ByteString as BS
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr)
import           Foreign.Storable (Storable(..), peek)
import qualified System.IO.Unsafe as Unsafe

import Slurm.Internal

#include <slurm/slurm.h>

data DynamicPluginData

newtype NodeState = NodeState Word32
  deriving (Storable, Show)
#enum NodeState, NodeState, \
  NODE_STATE_UNKNOWN,	\
  NODE_STATE_DOWN,	\
  NODE_STATE_IDLE,	\
  NODE_STATE_ALLOCATED,	\
  NODE_STATE_MIXED,     \
  NODE_STATE_RES,       \
  NODE_STATE_DRAIN,     \
  NODE_STATE_MAINT,     \
  NODE_STATE_REBOOT_REQUESTED

-- sort of a hacky, assymetric, broken "equals"
instance Eq NodeState where
  NodeState s == NodeState t
    | f == 0 = (s .&. #const NODE_STATE_BASE) == t
    | popCount f == 1 = s .&. t /= 0
    | otherwise = s == t
    where f = t .&. #const NODE_STATE_FLAGS

data NodeInfo = NodeInfo
  { nodeInfoName :: !NodeName
  , nodeInfoState :: !NodeState
  , nodeInfoCPUs :: !Word16
  , nodeInfoLoad :: Maybe Word32
  , nodeInfoBootTime :: !CTime
  , nodeInfoTRES
  , nodeInfoTRESAlloc :: !BS.ByteString
  , nodeInfoMem
  , nodeInfoMemFree :: !Word64
  , nodeInfoFeatures :: !BS.ByteString
  , nodeInfoReason :: !BS.ByteString
  } deriving (Show)

unknownNodeInfo :: NodeInfo
unknownNodeInfo = NodeInfo
  mempty
  nodeStateUnknown
  0
  Nothing
  0
  mempty
  mempty
  0
  0
  mempty
  mempty

foreign import ccall unsafe slurm_get_select_nodeinfo :: Ptr DynamicPluginData -> CInt -> CInt -> Ptr a -> IO CInt

getSelectNodeinfo :: Storable a => Ptr NodeInfo -> CInt -> CInt -> IO (Maybe a)
getSelectNodeinfo np dt st = alloca $ \p -> do
  dyn <- (#peek node_info_t, select_nodeinfo) np
  r <- slurm_get_select_nodeinfo dyn dt st p
  if r == 0
    then Just <$> peek p
    else return Nothing

instance Storable NodeInfo where
  sizeOf _    = #size node_info_t
  alignment _ = #alignment node_info_t
  peek p = NodeInfo
    <$> (packCString =<< (#peek node_info_t, name) p)
    <*> (#peek node_info_t, node_state) p
    <*> (#peek node_info_t, cpus) p
    <*> (mfilter (/= #const NO_VAL) . Just <$> (#peek node_info_t, cpu_load) p)
    <*> (#peek node_info_t, boot_time) p
    <*> (packCString =<< (#peek node_info_t, tres_fmt_str) p)
    <*> (do
      sp <- getSelectNodeinfo p (#const SELECT_NODEDATA_TRES_ALLOC_FMT_STR) (#const NODE_STATE_ALLOCATED)
      s <- maybe (return mempty) packCString sp
      mapM_ slurmFree sp
      return s)
    <*> (#peek node_info_t, real_memory) p
    <*> (#peek node_info_t, free_mem) p
    <*> (packCString =<< (#peek node_info_t, features) p)
    <*> (packCString =<< (#peek node_info_t, reason) p)
  poke = error "poke NodeInfo not implemented"

data NodeInfoMsg

peekNodes :: Ptr NodeInfoMsg -> IO (CTime, [NodeInfo])
peekNodes p = do
  t <- (#peek node_info_msg_t, last_update) p
  n <- (#peek node_info_msg_t, record_count) p
  (t, ) <$> (peekArray n =<< (#peek node_info_msg_t, node_array) p)

foreign import ccall unsafe slurm_load_node :: CTime -> Ptr (Ptr NodeInfoMsg) -> Word16 -> IO CInt
foreign import ccall unsafe "&slurm_free_node_info_msg" slurm_free_node_info_msg_ptr :: FunPtr (Ptr NodeInfoMsg -> IO ())

{-# NOINLINE nodeInfo #-}
nodeInfo :: MVar (ForeignPtr NodeInfoMsg)
nodeInfo = Unsafe.unsafePerformIO $ newMVar =<< newForeignPtr_ nullPtr

slurmLoadNodes :: IO (CTime, [NodeInfo])
slurmLoadNodes = loadWhenChanged nodeInfo
  (\p r -> slurm_load_node p r (#const SHOW_ALL))
  slurm_free_node_info_msg_ptr
  (#peek node_info_msg_t, last_update)
  peekNodes
