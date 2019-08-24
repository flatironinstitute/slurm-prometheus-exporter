{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (second)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Network.HTTP.Types (methodGet, notFound404, methodNotAllowed405)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Slurm
import Prometheus
import Job

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
      [ (statsInfoUser, realToFrac statsInfoUserTime / 1000000 :: Double)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing

jobs :: Exporter
jobs = prefix "job" $ do
  (jt, jl) <- liftIO slurmLoadJobs
  let ja = accountJobs jl
  prefix "usage" $ do
    let f a n h = gauge n (Just h) (map (second a) ja) (Just $ realToFrac jt)
    f allocJob  "jobs"  "count of active jobs"
    f allocNode "nodes" "count of allocated nodes"
    f allocCPU  "cpus"  "count of allocated CPU cores"
    f allocMem  "bytes" "count of allocated memory"
    f allocGPU  "gpus"  "count of allocated GPUs"

exporters :: [(T.Text, Exporter)]
exporters =
  [ ("stats", stats)
  , ("jobs", jobs)
  , ("metrics", stats >> jobs)
  ]

main :: IO ()
main = do
  Warp.run 8090 $ \req resp ->
    case Wai.pathInfo req of
      [flip lookup exporters -> Just e]
        | Wai.requestMethod req == methodGet ->
          resp =<< response (prefix "slurm" e)
        | otherwise -> resp $ Wai.responseLBS methodNotAllowed405 [] mempty
      _ -> resp $ Wai.responseLBS notFound404 [] mempty
