{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Report
  ( slurmReport
  , historicalSlurmReports
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (second)
import           Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.List ((\\))
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Time.Calendar (Day, toGregorian, fromGregorian)
import           Data.Time.Clock (getCurrentTime, addUTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime (LocalTime(..), getTimeZone, TimeZone, localTimeToUTC, ZonedTime(..), getZonedTime, midnight, TimeOfDay(..), utcToZonedTime)
import           Foreign.C.Types (CTime(..))
import           System.IO (hPutStrLn, stderr)
import qualified System.IO.Unsafe as Unsafe

import Slurm
import Prometheus

-- because slurm does everything in local time, including daily rollups, we have to do a lot of LocalTime stuff

-- |Convert a LocalTime without a timezone to UTC, attempting to determine the local time zone at that time (if possible).
unzonedTimeToPOSIX :: TimeZone -> LocalTime -> IO POSIXTime
unzonedTimeToPOSIX z0 l = utcTimeToPOSIXSeconds <$> fixZone (0 :: Int) z0 where
  -- some sort of crazy fixed point? of course may not converge for boundaries
  fixZone i z = do
    z' <- getTimeZone t
    if z == z' then return t else if i >= 2 then do
      hPutStrLn stderr $ "unzonedTimeToPosix did not converge: " ++ show l
      return t
    else fixZone (succ i) z'
    where t = localTimeToUTC z l

parseTime :: LocalTime -> BS.ByteString -> LocalTime
parseTime now "now" = now
parseTime now "hour" = now{ localTimeOfDay = (localTimeOfDay now){ todMin = 0, todSec = 0} }
parseTime now "today" = now{ localTimeOfDay = midnight }
parseTime now "day" = now{ localTimeOfDay = midnight }
parseTime now "month" = let (y,m,_) = toGregorian (localDay now) in LocalTime (fromGregorian y m 1) midnight
parseTime now "year" = let (y,_,_) = toGregorian (localDay now) in LocalTime (fromGregorian y 1 1) midnight
parseTime now s = min now $ fromJust $
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
    al -> [("account",BSC.intercalate "," al)]

clusterLabels :: ReportClusterRec -> Labels
clusterLabels c = [("cluster", reportClusterRecName c)]

queryRange :: IO DBConn -> ZonedTime -> [BS.ByteString] -> (LocalTime, LocalTime) -> IO (POSIXTime, [ReportClusterRec])
queryRange dbc now cl (s, e) | s >= e = do
    ep <- unzonedTimeToPOSIX (zonedTimeZone now) e
    return (ep, [])
  | otherwise = do
  db <- dbc
  sp <- unzonedTimeToPOSIX (zonedTimeZone now) s
  ep <- unzonedTimeToPOSIX (zonedTimeZone now) e
  (,) ep <$> reportUserTopUsage db UserCond
    { userAssocCond = Just $ AssocCond
      { assocCondClusters = Just cl
      , assocCondUsageStart = CTime $ round sp
      , assocCondUsageEnd = CTime $ round ep
      }
    } True

data Cache a = Cache
  { cacheKey :: a
  , cacheClusters :: [BS.ByteString]
  , cacheReport :: (POSIXTime, [ReportClusterRec])
  }

newCache :: a -> Cache a
newCache a = Cache a [] (0, [])

cachedQueryRange :: Eq a => IO DBConn -> ZonedTime -> [BS.ByteString] -> MVar (Cache a) -> a -> (LocalTime, LocalTime) -> IO (POSIXTime, [ReportClusterRec])
cachedQueryRange db now clusters cachev key rng@(s,e)
  | s >= e = qr clusters
  | otherwise = do
    Cache{..} <- readMVar cachev
    if key == cacheKey then
      if clusters == cacheClusters then
        return cacheReport
      else if null clusters then
        set clusters =<< qr clusters
      else
        second (filter (\c -> reportClusterRecName c `elem` clusters) (snd cacheReport) ++) <$>
          case clusters \\ cacheClusters of
            [] -> return (fst cacheReport, [])
            dl -> do
              r <- qr dl
              void $ set (cacheClusters ++ dl) (second (snd cacheReport ++) r)
              return r
    else
      set clusters =<< qr clusters
  where
  qr cl = queryRange db now cl rng
  set cl v = do
    void $ swapMVar cachev $ Cache key cl v
    return v

-- slurmdb is much faster to query (full days) + (today) separately than the full range, so cache the day portion
{-# NOINLINE dayCache #-}
dayCache :: MVar (Cache (Day, Day))
dayCache = Unsafe.unsafePerformIO $ newMVar $ newCache (toEnum 0,toEnum 0)

{-# NOINLINE timeCache #-}
timeCache :: MVar (Cache (Day, TimeOfDay))
timeCache = Unsafe.unsafePerformIO $ newMVar $ newCache (toEnum 0,midnight)

splitDays :: (LocalTime, LocalTime) -> Maybe (Day, Day, TimeOfDay)
splitDays (LocalTime d1 t1, LocalTime d2 t2)
  | t1 == midnight && d1 <= d2 = Just (d1, d2, t2)
  | otherwise = Nothing

reportUsage :: POSIXTime -> [ReportClusterRec] -> Exporter
reportUsage endp r = do
  prefix "cluster" $ usages (const []) reportClusterTRES return
  prefix "user"    $ usages userLabels reportUserTRES reportClusterUsers
  where
  usages alab arec aget = do
    usage "seconds" tresRecAllocSecs alab arec aget
    -- usage "count" tresRecCount    alab arec aget
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

slurmReport :: [BS.ByteString] -> IO DBConn -> Exporter
slurmReport clusters db = do
  delay <- asks (optReportDelay . promOpts)
  nowt <- addUTCTime (negate delay) <$> liftIO getCurrentTime
  nowz <- liftIO $ getTimeZone nowt
  let now = utcToZonedTime nowz nowt
  start <- queryTime now "year" "start"
  end <- queryTime now "hour" "end"
  let rng = (start, end)
  (t, r) <- liftIO $ maybe
    (queryRange db now clusters rng)
    (\(startd, endd, endt) -> do
      (_, dr) <- cachedQueryRange db now clusters dayCache  (startd, endd) (LocalTime startd midnight, LocalTime endd midnight)
      (t, tr) <- cachedQueryRange db now clusters timeCache (endd,   endt) (LocalTime endd   midnight, LocalTime endd endt)
      return (t, mergeLists dr tr))
    $ splitDays rng
  reportUsage (t + delay) r

historicalSlurmReports :: [BS.ByteString] -> IO DBConn -> Exporter
historicalSlurmReports clusters db = do
  now <- liftIO getZonedTime
  start <- queryTime now "year" "start"
  end <- queryTime now "hour" "end"
  hourly <- or <$> queryBool "hourly"
  maybe (fail "invalid time range") (\(startd, endd, endt) -> do
    let loopDays d
          | d > endd = return ()
          | otherwise = do
            liftIO $ print d
            (t, dr) <- liftIO $ queryRange db now clusters (start, LocalTime d midnight)
            reportUsage t dr
            when hourly $ loopHours d dr 1
            loopDays (succ d)
        loopHours d dr h
          | h > if d == endd then todHour endt else 23 = return ()
          | otherwise = do
            liftIO $ print (d, h)
            (t, tr) <- liftIO $ queryRange db now clusters (LocalTime d midnight, LocalTime d (TimeOfDay h 0 0))
            reportUsage t $ mergeLists dr tr
            loopHours d dr (succ h)
    loopDays startd) $ splitDays (start, end)
