{-# LANGUAGE OverloadedStrings #-}

module Report
  ( slurmReport
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.List ((\\))
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Time.Calendar (Day, toGregorian, fromGregorian)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime (LocalTime(..), getTimeZone, TimeZone, localTimeToUTC, ZonedTime(..), getZonedTime, midnight, TimeOfDay)
import           Foreign.C.Types (CTime(..))
import qualified System.IO.Unsafe as Unsafe

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

queryTime :: Monad m => ZonedTime -> BS.ByteString -> BS.ByteString -> PrometheusT m LocalTime
queryTime now d q = do
  s <- fromMaybe d <$> query1 q
  return $ parseTime (zonedTimeToLocalTime now) s

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

-- slurmdb is much faster to query (full days) + (today) separately than the full range, so cache the day portion
{-# NOINLINE dayCache #-}
dayCache :: MVar (Day, Day, [ReportClusterRec])
dayCache = Unsafe.unsafePerformIO $ newMVar (toEnum 0,toEnum 0,[])

splitDays :: LocalTime -> LocalTime -> Maybe (Day, Day, TimeOfDay)
splitDays (LocalTime d1 t1) (LocalTime d2 t2)
  | t1 == midnight = Just (d1, d2, t2)
  | otherwise = Nothing

slurmReport :: [BS.ByteString] -> Exporter
slurmReport clusters = do
  now <- liftIO getZonedTime
  start <- queryTime now "year" "start"
  end <- queryTime now "now" "end"
  endp <- liftIO $ unzonedTimeToPOSIX (zonedTimeZone now) end
  r <- if start >= end then return [] else liftIO $ withDBConn $ \db -> do
    let queryRange cl s e = do
          sp <- unzonedTimeToPOSIX (zonedTimeZone now) s
          ep <- unzonedTimeToPOSIX (zonedTimeZone now) e
          reportUserTopUsage db UserCond
            { userAssocCond = Just $ AssocCond
              { assocCondClusters = Just cl
              , assocCondUsageStart = CTime $ round sp
              , assocCondUsageEnd = CTime $ round ep
              }
            } False
    maybe
      (queryRange clusters start end)
      (\(startd, endd, endt) -> do
        dr <- if startd >= endd then return [] else do
          (startdc, enddc, cr) <- readMVar dayCache
          let crd | startdc == startd && enddc == endd = cr
                  | otherwise = []
          dr <- (crd ++) <$> case clusters \\ map reportClusterRecName crd of
            [] | not (null clusters && null crd) -> return [] -- not exactly right
            diffc -> queryRange diffc (LocalTime startd midnight) (LocalTime endd midnight)
          unless (endd < enddc) $ void $ swapMVar dayCache (startd, endd, dr)
          return $ if null clusters then dr else filter (\c -> reportClusterRecName c `elem` clusters) dr
        tr <- if endt <= midnight then return [] else
          queryRange clusters (LocalTime endd midnight) (LocalTime endd endt)
        return $ mergeLists dr tr)
      $ splitDays start end
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
          (Just endp)
      usages :: (b -> Labels) -> (b -> [TRESRec]) -> (ReportClusterRec -> [b]) -> Exporter
      usages alab arec aget = do
        usage "seconds" tresRecAllocSecs alab arec aget
        -- usage "count" tresRecCount    alab arec aget
  prefix "cluster" $ usages (const []) reportClusterTRES return
  prefix "user"    $ usages userLabels reportUserTRES reportClusterUsers 
