module Base.Middleware.Trace where

import           Base.Web.Types
import           Boots
import           Control.Monad.Reader
import           Data.Opentracing
import           Data.Proxy
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Vault.Lazy      as L
import           Lens.Micro
import           Network.HTTP.Types
import           Network.Wai

hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

pluginTrace
  :: forall m cxt env n
  . ( HasWeb m cxt env
    , HasLogger env
    , MonadIO n)
  => Proxy m -> Proxy cxt -> (forall a. (LogFunc -> LogFunc) -> m a -> m a) -> Plugin env n env
pluginTrace pm pc fx = do
  key <- liftIO L.newKey
  spc <- liftIO newSpanContext
  env <- combine
    [ asks $ over (askWeb @m @cxt) $ \Web{..} -> Web{nature = \pc1 pm1 v ma -> nature pc1 pm1 v (fx (g $ L.lookup key v) ma), ..}
    , middlewarePlugin pm pc
        $ \app req resH -> do
          nspc <- case Prelude.lookup hTraceId (requestHeaders req) of
            Just htid -> case Prelude.lookup hSpanId (requestHeaders req) of
              Just hsid -> return $ Trace spc { traceId = htid } (SpanId hsid)
              _         -> return $ Trace spc { traceId = htid } NoSpan
            _         -> do
              htid <- gen spc
              return $ Trace spc { traceId = htid } NoSpan
          let name = decodeUtf8 (requestMethod req) <> " /" <> T.intercalate "/" (pathInfo req)
          runWithTrace name nspc
            $ \Trace{..} -> app req {vault = f (traceId context) spans key $ vault req }
            $ resH . mapResponseHeaders (\hs -> (hTraceId, traceId context):(hSpanId, r spans):hs)
    ]
  logInfo "Load plugin trace."
  return env
  where
    g (Just a) = addTrace a
    g _        = id
    f _   NoSpan      _ = id
    f tid (SpanId i)  k = L.insert k (decodeUtf8 $ tid <> "," <> i)
    f tid (SpanRef t) k = L.insert k (decodeUtf8 $ tid <> "," <> spanId t)
    r NoSpan      = ""
    r (SpanId  i) = i
    r (SpanRef t) = spanId t
