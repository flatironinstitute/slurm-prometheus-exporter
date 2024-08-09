{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first, second)
import           Control.Monad (when, mfilter, guard, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Fixed (Fixed(..), Micro)
import           Data.Functor (void)
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Foreign.C.Types (CTime)
import           Network.HTTP.Types (methodGet, notFound404, methodNotAllowed405)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Posix.Time (epochTime)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)

import Slurm
import Prometheus
import TRES
import Job
import Node
import Report

getOpt :: Monad m => (Options -> Bool) -> BS.ByteString -> PrometheusT m Bool
getOpt o q = maybe (asks (o . promOpts)) return =<< queryBool q

type SlurmExporter = IO DBConn -> Exporter

stats :: Exporter
stats = prefix "stats" $ do
  StatsInfoResponse{..} <- liftIO slurmGetStatistics
  one gauge   "server_threads" Nothing [] statsInfoServerThreadCount (Just $ realToFrac statsInfoReqTime)
  prefix "jobs" $ do
    counter "total" Nothing (labeled "state"
      [ ("submitted", statsInfoJobsSubmitted)
      , ("started",   statsInfoJobsStarted)
      , ("completed", statsInfoJobsCompleted)
      , ("canceled",  statsInfoJobsCanceled)
      , ("failed",    statsInfoJobsFailed)
      ]) Nothing
    gauge "count" Nothing (labeled "state"
      [ ("pending", statsInfoJobsPending)
      , ("running", statsInfoJobsRunning)
      ]) (realToFrac <$> statsInfoJobStatesTime)
  prefix "rpc_user" $ do
    counter "total"         Nothing (labeled "user"
      [ (statsInfoUser, statsInfoUserCnt)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing
    counter "seconds_total" Nothing (labeled "user"
      [ (statsInfoUser, MkFixed (toInteger statsInfoUserTime) :: Micro)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing

allocGauges :: (Eq a, Labeled a) => a -> Bool -> Bool -> CTime -> [(a, Labels, Alloc)] -> Exporter
allocGauges m c gputype _ la = do
  when c $ f    allocJob   lr "count" "count of active jobs"
  f (tresNode . allocTRES) lr "nodes" "count of allocated/requested nodes"
  f (tresCPU  . allocTRES) lr "cpus"  "count of allocated/requested CPU cores"
  f (tresMem  . allocTRES) lr "bytes" "total size of allocated/requested memory"
  (if gputype then g else f)
    (tresGPU  . allocTRES) lr "gpus"  "count of allocated/requested GPUs"
  f             allocTime  lr "seconds" "total job run/wait time"
  f             allocLoad  ls "load"  "total load of allocated nodes"
  f             allocMem   ls "used_bytes" "total size of used memory"
  where
  mkg l n h = gauge n (Just h) l Nothing
  f a l = mkg (second a <$> l)
  g _ l = mkg (foldMap gpulab l)
  ls = unlab la
  lr = map lab la
  unlab ((x, l, r) : s) | x == m = (l, r) : unlab s
  unlab _ = []
  lab (a, l, r) = (("state", label a) : l, r)
  gpulab (l, a)
    | not (null gs) = gs
    | gn /= 0 = [(l, gn)]
    | otherwise = []
    where
    gs = map (first $ (: l) . (,) "gputype") $ Map.toList $ gresMap $ allocGPUs a
    gn = tresGPU $ allocTRES a

jobs :: (CTime, [Node]) -> Exporter
jobs (nt, nl) = prefix "job" $ do
  (jt, jil) <- liftIO slurmLoadJobs
  now <- liftIO epochTime
  let nm = nodeMap nl
      jl = map (jobFromInfo now nm) jil
  withids <- getOpt optJobId "jobids"
  nodelist <- getOpt optNodelist "nodelist"
  gputype <- getOpt optGpuType "gputypes"
  allocGauges JobRunning True gputype (max nt jt) $ accountJobs withids nodelist jl
  -- TODO: sacct completed?

nodes :: PrometheusT IO (CTime, [Node])
nodes = prefix "node" $ do
  (nt, nil) <- liftIO slurmLoadNodes
  now <- liftIO epochTime
  let nl = map (nodeFromInfo now) nil
  withreason <- getOpt optReason "reasons"
  gputype <- getOpt optGpuType "gputypes"
  allocGauges ResAlloc False gputype nt $ accountNodes withreason nl
  return (nt, nl)

report :: Bool -> SlurmExporter
report al db = do
  rl <- query (if al then "cluster" else "report")
  hist <- or <$> queryBool "historical"
  cl <- if null rl then asks (optReportClusters . promOpts) else return (mapMaybe (mfilter (not . BS.null)) rl)
  when (al || not (null cl)) $ prefix "report" $ (if hist then historicalSlurmReports else slurmReport) cl db

qos :: SlurmExporter
qos dbc = prefix "qos" $ do
  db <- liftIO dbc
  ptres <- liftIO $ parseTRESStr <$> getTRES db -- could probably be cached, whatever
  qosl <- liftIO $ getQOS db
  forM_ [("grptres", qosGrpTRES), ("maxtrespu", qosMaxTRESPU)] $ \(trest, qtres) -> prefix trest $ do
    mapM_ (\(t, v) -> gauge t Nothing v Nothing) $ Map.toList $ foldl' (\m' q ->
      foldl' (\m (t, n) ->
        Map.insertWith (++) (tresRecType t)
          [ ([("qos", qosRecName q)] ++
              (("gres", tresRecName t) <$ guard (tresRecType t == "gres"))
            , n)
          ] m)
        m' (ptres (qtres q)))
      Map.empty qosl
    

exporters :: [(T.Text, SlurmExporter)]
exporters =
  [ ("probe", \_ -> return ())
  , ("stats", \_ -> stats)
  , ("nodes", \_ -> void nodes)
  , ("jobs", \_ -> jobs (0, []))
  , ("report", report True)
  , ("qos", qos)
  , ("metrics", \db -> stats >> qos db >> nodes >>= jobs >> report False db)
  ]

defOptions :: Options
defOptions = Options
  { optPort = 8090
  , optOpenMetrics = False
  , optReason = False
  , optJobId = False
  , optGpuType = False
  , optNodelist = False
  , optReportClusters = []
  , optReportDelay = 0
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "p" ["port"]
      (Opt.ReqArg (\p o -> o{ optPort = (read p) }) "PORT")
      ("listen on port [" ++ show (optPort defOptions) ++ "]")
  , Opt.Option "r" ["reasons"]
      (Opt.NoArg (\o -> o{ optReason = True }))
      ("include node drain reasons by default (may increase prometheus database size)")
  , Opt.Option "j" ["jobids"]
      (Opt.NoArg (\o -> o{ optJobId = True }))
      ("include job ids by default (will increase prometheus database size)")
  , Opt.Option "N" ["nodelist"]
      (Opt.NoArg (\o -> o{ optNodelist = True }))
      ("include job node list instead of label by default (will increase prometheus database size)")
  , Opt.Option "G" ["gputypes"]
      (Opt.NoArg (\o -> o{ optGpuType = True }))
      ("include gpu types in slurm_*_gpus by default")
  , Opt.Option "c" ["report"]
      (Opt.ReqArg (\c o -> o{ optReportClusters = BSC.pack c : optReportClusters o }) "CLUSTER")
      "include sreport data from CLUSTER by default (may be repeated)"
  , Opt.Option "d" ["delay"]
      (Opt.ReqArg (\d o -> o{ optReportDelay = secondsToNominalDiffTime (read d) }) "SECs")
      "offset report data by SEC seconds to avoid rollup discontinuities"
  , Opt.Option "o" ["open-metrics"]
      (Opt.NoArg (\o -> o{ optOpenMetrics = True }))
      ("export in OpenMetrics format (rather than prometheus text)")
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  opts <- case Opt.getOpt Opt.Permute options args of
    (o, [], []) -> return $ foldl (flip ($)) defOptions o
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTIONS]\n") options
      exitFailure
  withSlurm Nothing $ Warp.run (optPort opts) $ \req resp ->
    case Wai.pathInfo req of
      [flip lookup exporters -> Just e]
        | Wai.requestMethod req == methodGet -> withLazyDBConn $ \db ->
          resp $ response opts req (prefix "slurm" $ e db)
        | otherwise -> resp $ Wai.responseLBS methodNotAllowed405 [] mempty
      _ -> resp $ Wai.responseLBS notFound404 [] mempty
