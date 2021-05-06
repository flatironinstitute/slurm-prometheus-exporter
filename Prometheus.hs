{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Prometheus
  ( Options(..)
  , PromData(..)
  , PrometheusT
  , Exporter
  , Label
  , Labels
  , Labeled(..)
  , counter
  , gauge
  , one
  , labeled
  , prefix
  , response
  , query
  , query1
  , queryBool
  ) where

import           Control.Monad (join, when)
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM)
import           Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import           Data.Fixed (showFixed)
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Network.HTTP.Types (hContentType, ok200)
import qualified Network.Wai as Wai

data Options = Options
  { optPort :: Int
  , optOpenMetrics :: Bool
  , optReason, optJobId, optNodelist :: Bool
  , optReportClusters :: [BS.ByteString]
  , optReportDelay :: NominalDiffTime
  }

data PromData = PromData
  { promOpts :: Options
  , promRequest :: Wai.Request
  , promSend :: B.Builder -> IO ()
  , promPrefix :: B.Builder
  }

newtype PrometheusT m a = PrometheusT{ runPrometheusT :: ReaderT PromData m a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader PromData, MonadFail, MonadTrans, MonadTransControl)

type Exporter = PrometheusT IO ()

instance MonadBase b m => MonadBase b (PrometheusT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (PrometheusT m) where
  type StM (PrometheusT m) a = ComposeSt PrometheusT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type Str = BS.ByteString
type Label = (Str, Str)
type Labels = [Label]

class Labeled a where
  label :: a -> Str

bc :: Char -> B.Builder
bc = B.char7

bs :: Str -> B.Builder
bs = B.byteString

labels :: Labels -> B.Builder
labels = blabs where
  blabs [] = mempty
  blabs (a:l) = bc '{' <> blab a <> foldMap ((bc ',' <>) . blab) l <> bc '}'
  blab (n,v) = bs n <> bc '=' <> bc '"' <> BP.primMapByteStringBounded esc v <> bc '"'
  esc = BP.condB (`BS.elem` "\"\n\\")
    (BP.liftFixedToBounded $ ('\\', ) BP.>$< BP.char7 BP.>*< BP.word8)
    (BP.liftFixedToBounded BP.word8)

metric :: Show val => Str -> String -> Maybe Str -> [(Maybe Str, Labels, val)] -> Maybe POSIXTime -> Exporter
metric nam typ help samples tim = PrometheusT $ ReaderT $ \dat -> promSend dat $
  let namb = promPrefix dat <> bs nam in
  B.string7 "# TYPE " <> namb <> sp <> B.string7 typ <> nl <>
  foldMap (\hel -> B.string7 "# HELP " <> namb <> sp <> bs hel <> nl) help <>
  foldMap (\(suf, lab, val) ->
    namb <> foldMap ((bc '_' <>) . bs) suf <> labels lab <> sp <> B.string7 (show val) <> ts (promOpts dat) <> nl)
    samples
  where
  sp = bc ' '
  nl = bc '\n'
  ts opts = foldMap ((sp <>) . if optOpenMetrics opts
    then (B.string7 . showFixed True . nominalDiffTimeToSeconds)
    else (B.integerDec . round . (1000 *)))
    tim

counter :: Show val => Str -> Maybe Str -> [(Labels, val)] -> Maybe POSIXTime -> Exporter
counter nam hel samp = metric nam "counter" hel [(Nothing, lab, val) | (lab, val) <- samp]

gauge :: Show val => Str -> Maybe Str -> [(Labels, val)] -> Maybe POSIXTime -> Exporter
gauge nam hel samp = metric nam "gauge" hel [(Nothing, lab, val) | (lab, val) <- samp]

one :: (Str -> Maybe Str -> [(Labels, val)] -> a) -> Str -> Maybe Str -> Labels -> val -> a
one f nam hel lab val = f nam hel [(lab, val)]

labeled :: Str -> [(Str, v)] -> [(Labels, v)]
labeled n s = [ ([(n,l)],v) | (l,v) <- s ]

prefix :: Str -> PrometheusT m a -> PrometheusT m a
prefix pre = PrometheusT . withReaderT (\dat -> dat{ promPrefix = promPrefix dat <> bs pre <> bc '_'}) . runPrometheusT

response :: Options -> Wai.Request -> Exporter -> Wai.Response
response opts req p =
  Wai.responseStream ok200
    [(hContentType, if optOpenMetrics opts
      then "application/openmetrics-text; version=1.0.0; charset=utf-8"
      else "text/plain; version=0.0.4")
    ]
    $ \send _ -> do
      runReaderT (runPrometheusT p) (PromData opts req send mempty)
      when (optOpenMetrics opts) $ send $ B.string7 "# EOF\n"

query :: Monad m => BS.ByteString -> PrometheusT m [Maybe BS.ByteString]
query q = asks $ map snd . filter ((q ==) . fst) . Wai.queryString . promRequest

query1 :: Monad m => BS.ByteString -> PrometheusT m (Maybe BS.ByteString)
query1 q = join . listToMaybe <$> query q

queryBool :: Monad m => BS.ByteString -> PrometheusT m (Maybe Bool)
queryBool q = fmap (all b) . listToMaybe <$> query q where
  b "" = False
  b "0" = False
  b _ = True
