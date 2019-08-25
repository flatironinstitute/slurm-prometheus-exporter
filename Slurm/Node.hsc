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
  , nodeStateReboot
  , nodeStateRes
  , NodeInfo(..)
  , unknownNodeInfo
  , slurmLoadNodes
  ) where

import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Monad (mfilter)
import           Data.Bits ((.&.), popCount)
import qualified Data.ByteString as BS
import           Data.Word (Word16, Word32)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr)
import           Foreign.Storable (Storable(..), peek)
import qualified System.IO.Unsafe as Unsafe

import Slurm.Internal

#include <slurm/slurm.h>

data DynamicPluginData

foreign import ccall unsafe slurm_get_select_nodeinfo :: Ptr DynamicPluginData -> CInt -> CInt -> Ptr a -> IO CInt

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
  NODE_STATE_REBOOT

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
  , nodeInfoTRES :: !BS.ByteString
  , nodeInfoTRESAlloc :: !BS.ByteString
  } deriving (Show)

unknownNodeInfo :: NodeInfo
unknownNodeInfo = NodeInfo
  mempty
  nodeStateUnknown
  0
  Nothing
  mempty
  mempty

instance Storable NodeInfo where
  sizeOf _    = #size node_info_t
  alignment _ = #alignment node_info_t
  peek p = NodeInfo
    <$> (packCString =<< (#peek node_info_t, name) p)
    <*> (#peek node_info_t, node_state) p
    <*> (#peek node_info_t, cpus) p
    <*> (mfilter (/= #const NO_VAL) . Just <$> (#peek node_info_t, cpu_load) p)
    <*> (packCString =<< (#peek node_info_t, tres_fmt_str) p)
    <*> (with nullPtr $ \datap -> do
      dyn <- (#peek node_info_t, select_nodeinfo) p
      _ <- slurm_get_select_nodeinfo dyn (#const SELECT_NODEDATA_TRES_ALLOC_FMT_STR) (#const NODE_STATE_ALLOCATED) datap
      sp <- peek datap
      s <- packCString sp
      slurmFree sp
      return s)
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
