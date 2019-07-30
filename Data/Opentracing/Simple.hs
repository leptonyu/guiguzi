module Data.Opentracing.Simple where

import           Control.Monad.State
import           Data.Opentracing.Tracer
import           Data.Opentracing.Types

newtype TracerT m a = TracerT { runTracer :: StateT (Maybe Span, SpanContext) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadIO m => MonadTracer (TracerT m) where
  askSpanContext = TracerT $ gets snd

instance MonadIO m => MonadTracing (TracerT m) where
  runInSpan name notify action = do
    (s,c) <- TracerT get
    n <- case s of
      Just sp -> newChildSpan name sp
      _       -> newSpan name
    TracerT $ put (Just n,c)
    notify n
    a <- action n
    finishSpan n >>= notify
    return a

type Tracer = TracerT IO

runTracing :: TracerT IO a -> IO a
runTracing a = do
  c <- newContext
  fst <$> runStateT (runTracer a) (Nothing, c)
