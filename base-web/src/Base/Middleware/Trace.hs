module Base.Middleware.Trace where

import           Base.Web.Config
import           Base.Web.Middleware
import           Boots
import           Control.Monad.IO.Class
import           Data.Opentracing
import           Data.Proxy
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Vault.Lazy        as L
import           Lens.Micro
import           Network.HTTP.Types
import           Network.Wai

hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

pluginTrace
  :: forall cxt env m n
  . ( HasNatureT cxt n env
    , HasLogger cxt
    , HasMiddleware env
    , MonadIO m
    , HasLogger env)
  => Proxy n -> Plugin env m env
pluginTrace _ = do
  logInfo "Load plugin trace."
  env <- ask
  key <- liftIO L.newKey
  spc <- liftIO newSpanContext
  promote (modifyNT' @n (\(v,c) -> (v, g (L.lookup key v) c)) env)
    $ middlewarePlugin
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
  where
    g :: Maybe T.Text -> cxt -> cxt
    g Nothing  cxt = cxt
    g (Just s) cxt = over askLogger (addTrace s) cxt
    f _   NoSpan      _ = id
    f tid (SpanId i)  k = L.insert k (decodeUtf8 $ tid <> "," <> i)
    f tid (SpanRef t) k = L.insert k (decodeUtf8 $ tid <> "," <> spanId t)
    r NoSpan      = ""
    r (SpanId  i) = i
    r (SpanRef t) = spanId t
