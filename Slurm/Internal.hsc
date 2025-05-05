{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Slurm.Internal where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar)
import           Control.Exception (bracket)
import           Control.Monad ((<=<), mfilter)
import qualified Data.ByteString as BS
import           Data.Foldable (fold)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Error (Errno(..), getErrno)
import           Foreign.C.String (CString, peekCString, withCString)
import           Foreign.C.Types (CInt(..), CTime(..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr, FinalizerPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes, allocaBytesAligned)
import           Foreign.Marshal.Utils (with, maybePeek, fillBytes)
import           Foreign.Ptr (Ptr, FunPtr, nullPtr, nullFunPtr)
import           Foreign.Storable (Storable(..), peek)
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

foreign import ccall unsafe slurm_strerror :: Errno -> CString

checkError :: IO CInt -> IO (Maybe Errno)
checkError f = do
  e <- f
  if e == (#const SLURM_SUCCESS) then return Nothing else Just <$> getErrno

throwError :: Errno -> IO ()
throwError = ioError . userError <=< peekCString . slurm_strerror

throwIfError :: IO CInt -> IO ()
throwIfError = mapM_ throwError <=< checkError

calloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
calloca f = alloca $ \p -> do
  fillBytes p 0 $ sizeOf (undefined :: a)
  f p

callocaBytes :: Int -> (Ptr a -> IO b) -> IO b
callocaBytes n f = allocaBytes n $ \p -> do
  fillBytes p 0 n
  f p

callocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
callocaBytesAligned n a f = allocaBytesAligned n a $ \p -> do
  fillBytes p 0 n
  f p

data XList a
type List a = Ptr (XList a)
data XListIterator a
type ListIterator a = Ptr (XListIterator a)

foreign import ccall unsafe slurm_list_create :: FunPtr (Ptr a -> IO ()) -> IO (List a)
foreign import ccall unsafe slurm_list_append :: List a -> Ptr a -> IO (Ptr a)
foreign import ccall   safe slurm_list_destroy :: List a -> IO ()
foreign import ccall unsafe slurm_list_iterator_create :: List a -> IO (ListIterator a)
foreign import ccall unsafe slurm_list_iterator_destroy :: ListIterator a -> IO ()
foreign import ccall unsafe slurm_list_next :: ListIterator a -> IO (Ptr a)

withListIterator :: List a -> (ListIterator a -> IO b) -> IO b
withListIterator l = bracket
  (slurm_list_iterator_create l)
  slurm_list_iterator_destroy

fromList :: List a -> IO [Ptr a]
fromList l
  | l == nullPtr = return []
  | otherwise = withListIterator l iter where
    iter i = do
      p <- slurm_list_next i
      if p == nullPtr
        then return []
        else (p :) <$> iter i

peekList :: Storable a => List a -> IO [a]
peekList l = mapM peek =<< fromList l

-- |result must be freed with slurm_list_destroy
createList :: FinalizerPtr a -> [Ptr a] -> IO (List a)
createList f l = do
  p <- slurm_list_create f
  mapM_ (slurm_list_append p) l
  return p

withList :: [Ptr a] -> (List a -> IO b) -> IO b
withList l = bracket
  (createList nullFunPtr l)
  slurm_list_destroy

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
      (\e -> info <$ if e == Errno (#const SLURM_NO_CHANGE_IN_DATA)
        then return ()
        else throwError e)
      err
    (info', ) <$> withForeignPtr info' f
