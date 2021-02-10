{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow ((&&&), second)
import           Control.Monad (when, mfilter)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Fixed (Fixed(..), Micro)
import           Data.Functor (void)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
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
      ]) (Just $ realToFrac statsInfoJobStatesTime)
  prefix "rpc_user" $ do
    counter "total"         Nothing (labeled "user"
      [ (statsInfoUser, statsInfoUserCnt)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing
    counter "seconds_total" Nothing (labeled "user"
      [ (statsInfoUser, MkFixed (toInteger statsInfoUserTime) :: Micro)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing

allocGauges :: (Eq a, Labeled a) => a -> Bool -> CTime -> [(a, Labels, Alloc)] -> Exporter
allocGauges m c _ la = do
  when c $ f    allocJob   lr "count" "count of active jobs"
  f (tresNode . allocTRES) lr "nodes" "count of allocated/requested nodes"
  f (tresCPU  . allocTRES) lr "cpus"  "count of allocated/requested CPU cores"
  f (tresMem  . allocTRES) lr "bytes" "total size of allocated/requested memory"
  f (tresGPU  . allocTRES) lr "gpus"  "count of allocated/requested GPUs"
  f             allocTime  lr "seconds" "total job run/wait time"
  f             allocLoad  ls "load"  "total load of allocated nodes"
  f             allocMem   ls "used_bytes" "total size of used memory"
  where
  f a l n h = gauge n (Just h) (second a <$> l) Nothing
  (ls, lr) = (unlab &&& map lab) la
  unlab ((x, l, r) : s) | x == m = (l, r) : unlab s
  unlab _ = []
  lab (a, l, r) = (("state", label a) : l, r)

jobs :: (CTime, [Node]) -> Exporter
jobs (nt, nl) = prefix "job" $ do
  (jt, jil) <- liftIO slurmLoadJobs
  now <- liftIO epochTime
  let nm = nodeMap nl
      jl = map (jobFromInfo now nm) jil
  withids <- maybe (asks (optJobId . promOpts)) return =<< queryBool "jobids"
  allocGauges JobRunning True (max nt jt) $ accountJobs withids jl
  -- TODO: sacct completed?

nodes :: PrometheusT IO (CTime, [Node])
nodes = prefix "node" $ do
  (nt, nil) <- liftIO slurmLoadNodes
  now <- liftIO epochTime
  let nl = map (nodeFromInfo now) nil
  withreason <- maybe (asks (optReason . promOpts)) return =<< queryBool "reasons"
  allocGauges ResAlloc False nt $ accountNodes withreason nl
  return (nt, nl)

report :: Bool -> Exporter
report al = do
  rl <- query (if al then "cluster" else "report")
  cl <- if null rl then asks (optReportClusters . promOpts) else return (mapMaybe (mfilter (not . BS.null)) rl)
  when (al || not (null cl)) $ prefix "report" $ slurmReport cl

exporters :: [(T.Text, Exporter)]
exporters =
  [ ("probe", return ())
  , ("stats", stats)
  , ("nodes", void nodes)
  , ("jobs", jobs (0, []))
  , ("report", report True)
  , ("metrics", stats >> nodes >>= jobs >> report False)
  ]

defOptions :: Options
defOptions = Options
  { optPort = 8090
  , optOpenMetrics = False
  , optReason = False
  , optJobId = False
  , optReportClusters = []
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
      ("include job ids by default (may increase prometheus database size)")
  , Opt.Option "c" ["report"]
      (Opt.ReqArg (\c o -> o{ optReportClusters = BSC.pack c : optReportClusters o }) "CLUSTER")
      "include sreport data from CLUSTER by default (may be repeated)"
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
  Warp.run (optPort opts) $ \req resp ->
    case Wai.pathInfo req of
      [flip lookup exporters -> Just e]
        | Wai.requestMethod req == methodGet ->
          resp =<< response opts req (prefix "slurm" e)
        | otherwise -> resp $ Wai.responseLBS methodNotAllowed405 [] mempty
      _ -> resp $ Wai.responseLBS notFound404 [] mempty
