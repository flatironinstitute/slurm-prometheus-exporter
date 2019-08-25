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
      [ (statsInfoUser, realToFrac statsInfoUserTime / 1000000 :: Double)
      | StatsInfoUser{..} <- statsInfoRpcUser]) Nothing

jobs :: Exporter
jobs = prefix "job" $ do
  (jt, jl) <- liftIO slurmLoadJobs
  let ja = accountJobs jl
  prefix "usage" $ do
    let f a n h = gauge n (Just h) (map (second a) ja) (Just $ realToFrac jt)
    f             allocJob   "jobs"  "count of active jobs"
    f (tresNode . allocTRES) "nodes" "count of allocated nodes"
    f (tresCPU  . allocTRES) "cpus"  "count of allocated CPU cores"
    f (tresMem  . allocTRES) "bytes" "count of allocated memory"
    f (tresGPU  . allocTRES) "gpus"  "count of allocated GPUs"

nodes :: Exporter
nodes = prefix "node" $ do
  (nt, nl) <- liftIO slurmLoadNodes
  let na = accountNodes nl
  prefix "usage" $ do
    let f a n = gauge n Nothing (map (second a) na) (Just $ realToFrac nt)
    f tresNode "nodes" 
    f tresCPU  "cpus"  
    f tresMem  "bytes" 
    f tresGPU  "gpus" 

exporters :: [(T.Text, Exporter)]
exporters =
  [ ("stats", stats)
  , ("jobs", jobs)
  , ("nodes", nodes)
  , ("metrics", stats >> jobs >> nodes)
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
