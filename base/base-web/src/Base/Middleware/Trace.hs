module Base.Middleware.Trace where

import           Base.Web.Types
import           Boots
import           Data.Opentracing
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import           Lens.Micro.Extras
import           Network.HTTP.Types
import           Network.Wai

{-# INLINE hTraceId #-}
hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

{-# INLINE hSpanId #-}
hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

buildTrace
  ::( HasWeb m cxt env
    , HasLogger env
    , HasApp cxt env
    , MonadIO n)
  => Factory n env env
buildTrace = do
  AppEnv{..} <- asks (view askApp)
  logFunc    <- asks (view askLogger)
  spc <- liftIO $ newSpanContext $ rand64 randSeed
  logInfo "Load plugin trace."
  buildMiddleware $ \app req resH -> do
    nspc <- case Prelude.lookup hTraceId (requestHeaders req) of
      Just htid -> case Prelude.lookup hSpanId (requestHeaders req) of
        Just hsid -> return $ Trace spc { traceId = htid } (SpanId hsid)
        _         -> return $ Trace spc { traceId = htid } NoSpan
      _         -> do
        htid <- gen spc
        return $ Trace spc { traceId = htid } NoSpan
    let nm = decodeUtf8 (requestMethod req) <> " /" <> T.intercalate "/" (pathInfo req)
    runWithTrace nm nspc
      $ \Trace{..} -> do
        let req1 = req {vault = addTrace (f spans $ traceId context) logFunc $ vault req }
        app req1 $ \res -> do
          toLog logFunc req1 (responseStatus res) Nothing
          resH $ mapResponseHeaders (\hs -> (hTraceId, traceId context):(hSpanId, r spans):hs) res
  where
    {-# INLINE f #-}
    f NoSpan      _   = Nothing
    f (SpanId i)  tid = Just $ decodeUtf8 $ tid <> "," <> i
    f (SpanRef t) tid = Just $ decodeUtf8 $ tid <> "," <> spanId t
    {-# INLINE r #-}
    r NoSpan      = ""
    r (SpanId  i) = i
    r (SpanRef t) = spanId t
