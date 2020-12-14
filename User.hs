{-# LANGUAGE TupleSections #-}

module User
  ( UserName
  , uidName
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap.Strict as Map
import           Data.Word (Word32)
import           System.IO.Error (catchIOError)
import qualified System.IO.Unsafe as Unsafe
import           System.Posix.User (getUserEntryForID, userName)
import           System.Posix.Types (CUid(..))

type UserName = BS.ByteString

{-# NOINLINE uidCache #-}
uidCache :: MVar (Map.IntMap UserName)
uidCache = Unsafe.unsafePerformIO $ newMVar Map.empty

uidName :: Maybe UserName -> Word32 -> IO UserName
uidName mname uid =
  modifyMVar uidCache $ \cache -> maybe
    (maybe (do
        name <- BSC.pack <$> catchIOError (userName <$> getUserEntryForID (CUid uid)) (\_ -> return $ show uid)
        return (Map.insert key name cache, name))
      (return . (cache, )) $ Map.lookup key cache)
    (\name -> return (Map.insert key name cache, name))
    mname
  where key = fromIntegral uid
