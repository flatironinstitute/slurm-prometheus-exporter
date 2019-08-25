module Slurm.Hostlist
  ( NodeName
  , expandHostList'
  , expandHostList
  ) where

import           Control.Exception (bracket)
import qualified Data.ByteString as BS
import           Data.Function (fix)
import           Foreign.C.String (CString)
import           Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (free)
import           Foreign.Ptr (Ptr, FunPtr)
import qualified System.IO.Unsafe as Unsafe

import Slurm.Internal

#include <slurm/slurm.h>

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

