{-# LANGUAGE OverloadedStrings #-}

module Report
  ( slurmReport
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Time.Calendar (toGregorian, fromGregorian)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime (LocalTime(..), getTimeZone, TimeZone, localTimeToUTC, ZonedTime(..), getZonedTime, midnight)
import           Foreign.C.Types (CTime(..))

import Slurm
import Prometheus

unzonedTimeToPOSIX :: TimeZone -> LocalTime -> IO POSIXTime
unzonedTimeToPOSIX z0 l = do
  z <- getTimeZone (localTimeToUTC z0 l)
  -- just hope doesn't cross boundary...
  return $ utcTimeToPOSIXSeconds $ localTimeToUTC z l

parseTime :: LocalTime -> BS.ByteString -> LocalTime
parseTime now "now" = now
parseTime now "today" = LocalTime (localDay now) midnight
parseTime now "day" = LocalTime (localDay now) midnight
parseTime now "month" = let (y,m,_) = toGregorian (localDay now) in LocalTime (fromGregorian y m 1) midnight
parseTime now "year" = let (y,_,_) = toGregorian (localDay now) in LocalTime (fromGregorian y 1 1) midnight
parseTime _ s = fromJust $
  try df <|> try dt <|> try (dt <> ":%S")
  where
  try f = parseTimeM True defaultTimeLocale f s'
  s' = BSC.unpack s
  df = "%Y-%-m-%-d"
  dt = df <> "T%-H:%M"

queryTime :: ZonedTime -> BS.ByteString -> BS.ByteString -> PrometheusT IO POSIXTime
queryTime now d q = do
  s <- fromMaybe d <$> query1 q
  liftIO $ unzonedTimeToPOSIX (zonedTimeZone now) $ parseTime (zonedTimeToLocalTime now) s

tresLabels :: TRESRec -> Labels
tresLabels t = [("tres",
  (if BS.null (tresRecName t) then id else (<> ('/' `BSC.cons` tresRecName t)))
    $ tresRecType t)]

userLabels :: ReportUserRec -> Labels
userLabels u = ("user", reportUserRecName u) :
  case filter (not . BS.null) (reportUserRecAcct u : reportUserRecAccts u) of
    [] -> []
    (a:_) -> [("account",a)]

clusterLabels :: ReportClusterRec -> Labels
clusterLabels c = [("cluster", reportClusterRecName c)]

slurmReport :: [BS.ByteString] -> Exporter
slurmReport clusters = do
  now <- liftIO getZonedTime
  -- slurm goes faster if you query entire days
  -- default to midnight jan 1 this year until midnight this morning
  start <- queryTime now "year" "start"
  end <- queryTime now "day" "end"
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
          [ (tl, m)
          | c <- r
          , let cl = clusterLabels c
          , a <- aget c
          , let al = alab a ++ cl
          , t <- arec a
          , let m = tmet t
          , 0 /= m
          , let tl = tresLabels t ++ al
          ]
          (Just end)
      usages :: (b -> Labels) -> (b -> [TRESRec]) -> (ReportClusterRec -> [b]) -> Exporter
      usages alab arec aget = do
        usage "seconds" tresRecAllocSecs alab arec aget
        usage "count"   tresRecCount     alab arec aget
  prefix "cluster" $ usages (const []) reportClusterTRES return
  prefix "user"    $ usages userLabels reportUserTRES reportClusterUsers 
