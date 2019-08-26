{-# LANGUAGE TupleSections #-}

module Slurm.Internal where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar)
import           Control.Monad ((<=<), mfilter)
import qualified Data.ByteString as BS
import           Data.Foldable (fold)
import           Data.Maybe (fromMaybe)
import           Foreign.C.String (CString, peekCString, withCString)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Utils (with, maybePeek)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr)
import           Foreign.Storable (Storable(..), peek)
import           System.IO.Error (ioError, userError)
import qualified System.IO.Unsafe as Unsafe

#include <slurm/slurm.h>

foreign import ccall unsafe slurm_xfree :: Ptr (Ptr a) -> CString -> CInt -> CString -> IO ()

slurmFree :: Ptr a -> IO ()
slurmFree p = with p $ \pp ->
  withCString "Slurm.hsc" $ \file ->
  withCString "slurmFree" $ \func ->
  slurm_xfree pp file (-1) func

maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr = mfilter (/= nullPtr) . Just

maybeCString :: CString -> IO (Maybe BS.ByteString)
maybeCString = maybePeek BS.packCString

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

loadWhenChanged :: MVar (ForeignPtr a)
  -> (CTime -> Ptr (Ptr a) -> IO CInt)
  -> FunPtr (Ptr a -> IO ())
  -> (Ptr a -> IO CTime)
  -> (Ptr a -> IO b) -> IO b
loadWhenChanged mv load free lastup f = modifyMVar mv $ \info ->
  alloca $ \resp -> do
    prev <- withForeignPtr info $ fmap (fromMaybe 0) . maybePeek lastup
    err <- checkError $ load prev resp
    info' <- maybe
      (newForeignPtr free =<< peek resp)
      (\e -> info <$ if e == (#const SLURM_NO_CHANGE_IN_DATA)
        then return ()
        else throwError e)
      err
    (info', ) <$> withForeignPtr info' f
