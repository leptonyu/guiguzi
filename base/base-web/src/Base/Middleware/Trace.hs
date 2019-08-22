module Base.Middleware.Trace where

import           Base.Web.Types
import           Boots
import           Control.Monad
import           Data.Maybe
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
    , HasSalak env
    , HasApp cxt env
    , MonadIO n
    , MonadThrow n)
  => Factory n env env
buildTrace = do
  b <- fromMaybe True <$> require "trace.enabled"
  unless   b $ logInfo "Disable trace module"
  tryBuild b $ do
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
        $ \Trace{..} -> app req {vault = addTrace (f spans $ traceId context) logFunc $ vault req }
        $ resH . mapResponseHeaders (\hs -> (hTraceId, traceId context):(hSpanId, r spans):hs)
    where
      {-# INLINE f #-}
      f NoSpan      _   = Nothing
      f (SpanId i)  tid = Just $ decodeUtf8 $ tid <> "," <> i
      f (SpanRef t) tid = Just $ decodeUtf8 $ tid <> "," <> spanId t
      {-# INLINE r #-}
      r NoSpan      = ""
      r (SpanId  i) = i
      r (SpanRef t) = spanId t
