{-# LANGUAGE RecordWildCards #-}

module Slurm.DB
  ( DBConn
  , withDBConn
  , withLazyDBConn
  , AssocCond(..)
  , UserCond(..)
  , TRESRec(..)
  , ReportUserRec(..)
  , ReportClusterRec(..)
  , reportUserTopUsage
  , mergeLists
  ) where

import           Control.Arrow ((&&&))
import           Control.Concurrent.MVar (newMVar, takeMVar, modifyMVar)
import           Control.Exception (bracket)
import           Control.Monad ((<=<))
import qualified Data.ByteString as BS
import           Data.Function (on)
import           Data.List (sort)
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.Types (CInt(..), CBool(..), CTime(..))
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Marshal.Utils (with, maybeWith, withMany, fromBool)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(..))

import Slurm.Internal

#include <slurm/slurmdb.h>

data DBConnVal
type DBConn = Ptr DBConnVal

data AssocCond = AssocCond
  { assocCondClusters :: Maybe [BS.ByteString]
  , assocCondUsageEnd, assocCondUsageStart :: !CTime
  }

data UserCond = UserCond
  { userAssocCond :: Maybe AssocCond
  }

data TRESRec = TRESRec
  { tresRecId :: !Word32
  , tresRecAllocSecs :: !Word64
  --, tresRecRecCount :: !Word32
  --, tresRecCount :: !Word32
  , tresRecName :: !BS.ByteString
  , tresRecType :: !BS.ByteString
  } deriving (Show)

data ReportAssocRec = ReportAssocRec
  { reportAssocRecAcct :: !BS.ByteString
  , reportAssocRecUser :: !BS.ByteString
  , reportAssocTRES :: [TRESRec]
  } deriving (Show)

data ReportUserRec = ReportUserRec
  { reportUserRecName :: !BS.ByteString
  , reportUserRecAcct :: !BS.ByteString
  , reportUserRecAccts :: [BS.ByteString]
  , reportUserTRES :: [TRESRec]
  } deriving (Show)

data ReportClusterRec = ReportClusterRec
  { reportClusterRecName :: !BS.ByteString
  , reportClusterTRES :: [TRESRec]
  --, reportClusterAssocs :: [ReportAssocRec]
  , reportClusterUsers :: [ReportUserRec]
  } deriving (Show)

foreign import ccall unsafe slurmdb_connection_get :: Ptr Word16 -> IO DBConn
foreign import ccall unsafe slurmdb_connection_close :: Ptr DBConn -> IO CInt
foreign import ccall unsafe slurmdb_report_user_top_usage :: DBConn -> Ptr UserCond -> CBool -> IO (List ReportClusterRec)

getDBConn :: IO DBConn
getDBConn = throwIfNull "slurmdb_connection_get" (slurmdb_connection_get nullPtr)

withDBConn :: (DBConn -> IO a) -> IO a
withDBConn = bracket
  getDBConn
  (\p -> throwIfError $ with p slurmdb_connection_close)

bracketLazy :: IO a -> (a -> IO ()) -> (IO a -> IO b) -> IO b
bracketLazy get put run = bracket
  (newMVar Nothing)
  (mapM_ put <=< takeMVar)
  (\r -> run $ modifyMVar r (fmap (Just &&& id) . maybe get return))

withLazyDBConn :: (IO DBConn -> IO a) -> IO a
withLazyDBConn = bracketLazy
  getDBConn
  (\p -> throwIfError $ with p slurmdb_connection_close)

instance Storable AssocCond where
  sizeOf _    = #size slurmdb_assoc_cond_t
  alignment _ = #alignment slurmdb_assoc_cond_t
  peek = error "peek AssocCond not implemented"
  poke = error "poke AssocCond not implemented"

instance Storable UserCond where
  sizeOf _    = #size slurmdb_user_cond_t
  alignment _ = #alignment slurmdb_user_cond_t
  peek = error "peek UserCond not implemented"
  poke = error "poke UserCond not implemented"


withAssocCond :: AssocCond -> (Ptr AssocCond -> IO a) -> IO a
withAssocCond AssocCond{..} f = calloca $ \ap ->
  maybeWith (\cl -> withMany BS.useAsCString cl . flip withList)
    assocCondClusters $ \clp -> do
      (#poke slurmdb_assoc_cond_t, cluster_list) ap clp
      (#poke slurmdb_assoc_cond_t, usage_end) ap assocCondUsageEnd
      (#poke slurmdb_assoc_cond_t, usage_start) ap assocCondUsageStart
      f ap

withUserCond :: UserCond -> (Ptr UserCond -> IO a) -> IO a
withUserCond UserCond{..} f = calloca $ \up ->
  maybeWith withAssocCond userAssocCond $ \ap -> do
    (#poke slurmdb_user_cond_t, assoc_cond) up ap
    f up

instance Storable TRESRec where
  sizeOf _    = #size slurmdb_tres_rec_t
  alignment _ = #alignment slurmdb_tres_rec_t
  peek p = TRESRec
    <$> ((#peek slurmdb_tres_rec_t, id) p)
    <*> ((#peek slurmdb_tres_rec_t, alloc_secs) p)
    -- <*> ((#peek slurmdb_tres_rec_t, rec_count) p)
    -- <*> ((#peek slurmdb_tres_rec_t, count) p)
    <*> (packCString =<< (#peek slurmdb_tres_rec_t, name) p)
    <*> (packCString =<< (#peek slurmdb_tres_rec_t, type) p)
  poke = error "poke TRESRec not implemented"

instance Storable ReportAssocRec where
  sizeOf _    = #size slurmdb_report_assoc_rec_t
  alignment _ = #alignment slurmdb_report_assoc_rec_t
  peek p = ReportAssocRec
    <$> (packCString =<< (#peek slurmdb_report_assoc_rec_t, acct) p)
    <*> (packCString =<< (#peek slurmdb_report_assoc_rec_t, user) p)
    <*> (peekList =<< (#peek slurmdb_report_assoc_rec_t, tres_list) p)
  poke = error "poke ReportAssocRec not implemented"

instance Storable ReportUserRec where
  sizeOf _    = #size slurmdb_report_user_rec_t
  alignment _ = #alignment slurmdb_report_user_rec_t
  peek p = ReportUserRec
    <$> (packCString =<< (#peek slurmdb_report_user_rec_t, name) p)
    <*> (packCString =<< (#peek slurmdb_report_user_rec_t, acct) p)
    <*> (mapM packCString =<< fromList =<< (#peek slurmdb_report_user_rec_t, acct_list) p)
    <*> (peekList =<< (#peek slurmdb_report_user_rec_t, tres_list) p)
  poke = error "poke ReportUserRec not implemented"

instance Storable ReportClusterRec where
  sizeOf _    = #size slurmdb_report_cluster_rec_t
  alignment _ = #alignment slurmdb_report_cluster_rec_t
  peek p = ReportClusterRec
    <$> (packCString =<< (#peek slurmdb_report_cluster_rec_t, name) p)
    <*> (peekList =<< (#peek slurmdb_report_cluster_rec_t, tres_list) p)
    -- <*> (peekList =<< (#peek slurmdb_report_cluster_rec_t, assoc_list) p)
    <*> (peekList =<< (#peek slurmdb_report_cluster_rec_t, user_list) p)
  poke = error "poke ReportClusterRec not implemented"

reportUserTopUsage :: DBConn -> UserCond -> Bool -> IO [ReportClusterRec]
reportUserTopUsage db c g = withUserCond c $ \cp -> bracket
  (slurmdb_report_user_top_usage db cp (fromBool g))
  slurm_list_destroy
  peekList

-- merge lists, assiming both are sorted
mergeSortedLists :: (Semigroup a, Ord a) => [a] -> [a] -> [a]
mergeSortedLists = ml where
  ml [] l = l
  ml l [] = l
  ml aal@(a:al) bbl@(b:bl) = case compare a b of
    EQ -> a <> b : ml al bl
    LT -> a : ml al bbl
    GT -> b : ml aal bl

-- merge lists, efficiently if both are sorted
mergeLists :: (Semigroup a, Ord a) => [a] -> [a] -> [a]
mergeLists a b = mergeSortedLists (sort a) (sort b)

instance Eq TRESRec where
  (==) = on (==) tresRecId
instance Ord TRESRec where
  compare = on compare tresRecId
instance Semigroup TRESRec where
  a <> b = a{ tresRecAllocSecs = tresRecAllocSecs a + tresRecAllocSecs b }

instance Eq ReportUserRec where
  (==) = on (==) reportUserRecName
instance Ord ReportUserRec where
  compare = on compare reportUserRecName
instance Semigroup ReportUserRec where
  a <> b = a{ reportUserTRES = on mergeLists reportUserTRES a b }

instance Eq ReportClusterRec where
  (==) = on (==) reportClusterRecName
instance Ord ReportClusterRec where
  compare = on compare reportClusterRecName
instance Semigroup ReportClusterRec where
  a <> b = a
    { reportClusterTRES = on mergeLists reportClusterTRES a b
    , reportClusterUsers = on mergeSortedLists reportClusterUsers a b
    }
