{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Prometheus
  ( PrometheusT
  , Labels
  , Labeled(..)
  , counter
  , gauge
  , one
  , labeled
  , prefix
  , response
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import           Control.Monad.Trans.Writer (WriterT(..), tell, execWriterT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import           Data.Time.Clock.POSIX (POSIXTime)
import           Network.HTTP.Types (hContentType, ok200)
import qualified Network.Wai as Wai

newtype PrometheusT m a = PrometheusT{ runPrometheusT ::
  ReaderT B.Builder (WriterT B.Builder m) a }
  deriving (Monad, Applicative, Functor, MonadIO)

instance MonadTrans PrometheusT where
  lift = PrometheusT . lift . lift

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
labels [] = mempty
labels lab = bc '{' <>
  foldMap (\(n,v) -> bs n <> bc '=' <> bc '"' <> BP.primMapByteStringBounded esc v <> bc '"' <> bc ',') lab
  <> bc '}'
  where
  esc = BP.condB (`BS.elem` "\"\n\\")
    (BP.liftFixedToBounded $ ('\\', ) BP.>$< BP.char7 BP.>*< BP.word8)
    (BP.liftFixedToBounded BP.word8)

metric :: (Monad m, Show val) => Str -> String -> Maybe Str -> [(Maybe Str, Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
metric nam typ help samples tim = PrometheusT $ ReaderT $ \pre -> tell $
  let namb = pre <> bs nam in
  B.string7 "# TYPE " <> namb <> sp <> B.string7 typ <> nl <>
  foldMap (\hel -> B.string7 "# HELP " <> namb <> sp <> bs hel <> nl) help <>
  foldMap (\(suf, lab, val) ->
    namb <> foldMap ((bc '_' <>) . bs) suf <> labels lab <> sp <> B.string7 (show val) <> ts <> nl)
    samples
  where
  sp = bc ' '
  nl = bc '\n'
  ts = foldMap ((sp <>) . B.int64Dec . round . (1000 *)) tim

counter :: (Monad m, Show val) => Str -> Maybe Str -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
counter nam hel samp = metric nam "counter" hel [(Nothing, lab, val) | (lab, val) <- samp]

gauge :: (Monad m, Show val) => Str -> Maybe Str -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
gauge nam hel samp = metric nam "gauge" hel [(Nothing, lab, val) | (lab, val) <- samp]

one :: (Str -> Maybe Str -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()) -> Str -> Maybe Str -> Labels -> val -> Maybe POSIXTime -> PrometheusT m ()
one f nam hel lab val = f nam hel [(lab, val)]

labeled :: Str -> [(Str, v)] -> [(Labels, v)]
labeled n s = [ ([(n,l)],v) | (l,v) <- s ]

prefix :: Str -> PrometheusT m a -> PrometheusT m a
prefix pre = PrometheusT . withReaderT (<> (bs pre <> bc '_')) . runPrometheusT

response :: Monad m => PrometheusT m () -> m Wai.Response
response p =
  Wai.responseBuilder ok200 [(hContentType, "text/plain; version=0.0.4")]
    <$> execWriterT (runReaderT (runPrometheusT p) mempty)
