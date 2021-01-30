{-# LANGUAGE OverloadedStrings #-}

module Report
  ( slurmReport
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Time.Calendar (toGregorian, fromGregorian)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Time.LocalTime (LocalTime(..), getTimeZone, TimeZone, localTimeToUTC, ZonedTime(..), getZonedTime, midnight)
import           Foreign.C.Types (CTime(..))

import Slurm
import Prometheus

unzonedTimeToPOSIX :: TimeZone -> LocalTime -> IO POSIXTime
unzonedTimeToPOSIX z0 l = do
  z <- getTimeZone (localTimeToUTC z0 l)
  -- just hope doesn't cross boundary...
  return $ utcTimeToPOSIXSeconds $ localTimeToUTC z l

tresLabels :: TRESRec -> Labels
tresLabels t = [("tres",
  (if BS.null (tresRecName t) then id else (<> ('/' `BSC.cons` tresRecName t)))
    $ tresRecType t)]

userLabels :: ReportUserRec -> Labels
userLabels u = ("user", reportUserRecName u) :
  case filter (not . BS.null) (reportUserRecAcct u : reportUserRecAccts u) of
    [] -> []
    (a:_) -> [("account",a)]

clusterLabel :: ReportClusterRec -> Label
clusterLabel c = ("cluster", reportClusterRecName c)

slurmReport :: [String] -> Exporter
slurmReport clusters = do
  now <- liftIO getZonedTime
  -- slurm goes faster if you query entire days
  -- start at midnight jan 1 this year, local time
  start <- liftIO $ unzonedTimeToPOSIX (zonedTimeZone now) $
    let (y,_,_) = toGregorian (localDay $ zonedTimeToLocalTime now) in LocalTime (fromGregorian y 1 1) midnight
  -- end at midnight this morning
  end <- liftIO $ unzonedTimeToPOSIX (zonedTimeZone now) $
    LocalTime (localDay $ zonedTimeToLocalTime now) midnight
  r <- liftIO $ withDBConn $ \db -> do
    reportUserTopUsage db UserCond
      { userAssocCond = Just $ AssocCond
        { assocCondClusters = Just clusters
        , assocCondUsageStart = CTime $ round start
        , assocCondUsageEnd = CTime $ round end
        }
      } False
  let usage :: (Show a, Num a, Eq a) => BS.ByteString -> (TRESRec -> a) -> (b -> Labels) -> (b -> [TRESRec]) -> (ReportClusterRec -> [b]) -> Exporter
      usage tlab tmet alab arec aget = 
        counter ("usage_" <> tlab) Nothing
          [ (cl : al ++ tl, m)
          | c <- r
          , let cl = clusterLabel c
          , a <- aget c
          , let al = alab a
          , t <- arec a
          , let tl = tresLabels t
          , let m = tmet t
          , 0 /= m
          ]
          (Just end)
      usages :: (b -> Labels) -> (b -> [TRESRec]) -> (ReportClusterRec -> [b]) -> Exporter
      usages alab arec aget = do
        usage "seconds" tresRecAllocSecs alab arec aget
        usage "count"   tresRecCount     alab arec aget
  prefix "cluster" $ usages (const []) reportClusterTRES return
  prefix "user"    $ usages userLabels reportUserTRES reportClusterUsers 
