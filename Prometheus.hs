{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Prometheus
  ( PrometheusT
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word (Word8)
import           Network.HTTP.Types (hContentType, ok200)
import qualified Network.Wai as Wai

newtype PrometheusT m a = PrometheusT{ runPrometheusT ::
  ReaderT B.Builder (WriterT B.Builder m) a }
  deriving (Monad, Applicative, Functor, MonadIO)

instance MonadTrans PrometheusT where
  lift = PrometheusT . lift . lift

type Label = (T.Text, T.Text)
type Labels = [(T.Text, T.Text)]

c7 = B.char7
bt = TE.encodeUtf8Builder

labels :: Labels -> B.Builder
labels [] = mempty
labels lab = c7 '{' <>
  foldMap (\(n,v) -> bt n <> c7 '=' <> c7 '"' <> TE.encodeUtf8BuilderEscaped esc v <> c7 '"' <> c7 ',') lab
  <> c7 '}'
  where
  esc = BP.condB (`BS.elem` "\"\n\\")
    (BP.liftFixedToBounded $ ('\\', ) BP.>$< BP.char7 BP.>*< BP.word8)
    (BP.liftFixedToBounded BP.word8)

metric :: (Monad m, Show val) => T.Text -> String -> Maybe T.Text -> [(Maybe T.Text, Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
metric nam typ help samples tim = PrometheusT $ ReaderT $ \pre -> tell $
  let namb = pre <> bt nam in
  B.string7 "# TYPE " <> namb <> sp <> B.string7 typ <> nl <>
  foldMap (\hel -> B.string7 "# HELP " <> namb <> sp <> bt hel <> nl) help <>
  foldMap (\(suf, lab, val) ->
    namb <> foldMap ((c7 '_' <>) . bt) suf <> labels lab <> sp <> B.string7 (show val) <> ts <> nl)
    samples
  where
  sp = c7 ' '
  nl = c7 '\n'
  ts = foldMap ((sp <>) . B.int64Dec . round . (1000 *)) tim

counter :: (Monad m, Show val) => T.Text -> Maybe T.Text -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
counter nam hel samp = metric nam "counter" hel [(Nothing, lab, val) | (lab, val) <- samp]

gauge :: (Monad m, Show val) => T.Text -> Maybe T.Text -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()
gauge nam hel samp = metric nam "gauge" hel [(Nothing, lab, val) | (lab, val) <- samp]

one :: (T.Text -> Maybe T.Text -> [(Labels, val)] -> Maybe POSIXTime -> PrometheusT m ()) -> T.Text -> Maybe T.Text -> Labels -> val -> Maybe POSIXTime -> PrometheusT m ()
one f nam hel lab val = f nam hel [(lab, val)]

labeled :: T.Text -> [(T.Text, v)] -> [(Labels, v)]
labeled n s = [ ([(n,l)],v) | (l,v) <- s ]

prefix :: T.Text -> PrometheusT m a -> PrometheusT m a
prefix pre = PrometheusT . withReaderT (<> (bt pre <> c7 '_')) . runPrometheusT

response :: Monad m => PrometheusT m () -> m Wai.Response
response p =
  Wai.responseBuilder ok200 [(hContentType, "text/plain; version=0.0.4")]
    <$> execWriterT (runReaderT (runPrometheusT p) mempty)
