{-# LANGUAGE TupleSections #-}

module User
  ( uidName
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Data.IntMap.Strict as Map
import           Data.Word (Word32)
import           System.IO.Error (catchIOError)
import qualified System.IO.Unsafe as Unsafe
import           System.Posix.User (getUserEntryForID, userName)
import           System.Posix.Types (CUid(..))

type UserName = String

{-# NOINLINE uidCache #-}
uidCache :: MVar (Map.IntMap UserName)
uidCache = Unsafe.unsafePerformIO $ newMVar Map.empty

uidName :: Word32 -> IO UserName
uidName uid = modifyMVar uidCache $ \cache ->
  maybe (do
    name <- catchIOError (userName <$> getUserEntryForID (CUid uid)) (\_ -> return $ show uid)
    return (Map.insert key name cache, name))
    (return . (cache, )) $ Map.lookup key cache
  where key = fromIntegral uid
