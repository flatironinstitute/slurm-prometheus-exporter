{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Monad (mapAndUnzipM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import           Network.HTTP.Types (hContentType, methodGet, ok200, notFound404, methodNotAllowed405)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Slurm
import User
import Prometheus

type Exporter = PrometheusT IO ()

stats :: Exporter
stats = prefix "stats" $ do
  StatsInfoResponse{..} <- liftIO slurmGetStatistics
  one gauge   "server_thread_total" Nothing [] statsInfoServerThreadCount (Just $ realToFrac statsInfoReqTime)
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
    (cnt, time) <- liftIO $ mapAndUnzipM (\StatsInfoUser{..} -> do
        n <- T.pack <$> uidName statsInfoUserId
        return ((n, statsInfoUserCnt), (n, realToFrac statsInfoUserTime / 1000000 :: Double)))
      statsInfoRpcUser
    counter "total" Nothing (labeled "user" cnt) Nothing
    counter "seconds_total" Nothing (labeled "user" time) Nothing

exporters :: [(T.Text, Exporter)]
exporters =
  [ ("stats", stats)
  ]

main :: IO ()
main = Warp.run 8090 $ \req resp -> do
  case Wai.pathInfo req of
    [flip lookup exporters -> Just exp]
      | Wai.requestMethod req == methodGet ->
        resp =<< response (prefix "slurm" exp)
      | otherwise -> resp $ Wai.responseLBS methodNotAllowed405 [] mempty
    _ -> resp $ Wai.responseLBS notFound404 [] mempty
