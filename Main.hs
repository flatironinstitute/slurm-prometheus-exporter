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
allocGauges m c t la = do
  when c $ f    allocJob   lr "count" "count of active jobs"
  f (tresNode . allocTRES) lr "nodes" "count of allocated nodes"
  f (tresCPU  . allocTRES) lr "cpus"  "count of allocated CPU cores"
  f (tresMem  . allocTRES) lr "bytes" "total size of allocated memory"
  f (tresGPU  . allocTRES) lr "gpus"  "count of allocated GPUs"
  f             allocLoad  ls "load"  "total load of allocated nodes"
  f             allocMem   ls "used_bytes" "total size of used memory"
  where
  f a l n h = gauge n (Just h) (second a <$> l) (Just $ realToFrac t)
  (ls, lr) = (unlab &&& map lab) la
  unlab ((x, l, r) : s) | x == m = (l, r) : unlab s
  unlab _ = []
  lab (a, l, r) = (("state", label a) : l, r)

jobs :: [Node] -> Exporter
jobs nl = prefix "job" $ do
  (jt, jil) <- liftIO slurmLoadJobs
  let nm = nodeMap nl
      jl = map (jobFromInfo nm) jil
  allocGauges JobRunning True jt $ accountJobs jl
  -- TODO: sacct completed?

nodes :: PrometheusT IO [Node]
nodes = prefix "node" $ do
  (nt, nil) <- liftIO slurmLoadNodes
  let nl = map nodeFromInfo nil
  allocGauges ResAlloc False nt $ accountNodes nl
  return nl

-- TODO: sreport?

exporters :: [(T.Text, Exporter)]
exporters =
  [ ("stats", stats)
  , ("nodes", void nodes)
  , ("jobs", jobs [])
  , ("metrics", stats >> nodes >>= jobs)
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
