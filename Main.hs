{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow ((&&&), second)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Fixed (Fixed(..), Micro)
import           Data.Functor (void)
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

type Exporter = PrometheusT IO ()

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

jobs :: Options -> (CTime, [Node]) -> Exporter
jobs opts (nt, nl) = prefix "job" $ do
  (jt, jil) <- liftIO slurmLoadJobs
  now <- liftIO epochTime
  let nm = nodeMap nl
      jl = map (jobFromInfo now nm) jil
  allocGauges JobRunning True (max nt jt) $ accountJobs opts jl
  -- TODO: sacct completed?

nodes :: Options -> PrometheusT IO (CTime, [Node])
nodes opts = prefix "node" $ do
  (nt, nil) <- liftIO slurmLoadNodes
  now <- liftIO epochTime
  let nl = map (nodeFromInfo now) nil
  allocGauges ResAlloc False nt $ accountNodes opts nl
  return (nt, nl)

-- TODO: sreport?

exporters :: Options -> [(T.Text, Exporter)]
exporters opts =
  [ ("stats", stats)
  , ("nodes", void $ nodes opts)
  , ("jobs", jobs opts (0, []))
  , ("metrics", stats >> nodes opts >>= jobs opts)
  ]

defOptions :: Options
defOptions = Options
  { optPort = 8090
  , optReason = False
  , optJobId = False
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "p" ["port"]
      (Opt.ReqArg (\p o -> o{ optPort = (read p) }) "PORT")
      ("listen on port [" ++ show (optPort defOptions) ++ "]")
  , Opt.Option "r" ["reasons"]
      (Opt.NoArg (\o -> o{ optReason = True }))
      ("include node drain reasons (may increase prometheus database size)")
  , Opt.Option "j" ["jobids"]
      (Opt.NoArg (\o -> o{ optJobId = True }))
      ("include job ids (may increase prometheus database size)")
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
      [flip lookup (exporters opts) -> Just e]
        | Wai.requestMethod req == methodGet ->
          resp =<< response (prefix "slurm" e)
        | otherwise -> resp $ Wai.responseLBS methodNotAllowed405 [] mempty
      _ -> resp $ Wai.responseLBS notFound404 [] mempty
