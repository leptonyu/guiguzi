module Base.Middleware.Logger where

import           Base.Metrics
import           Base.Web.Types
import           Boots
import           Lens.Micro.Extras
import           Network.Wai
import           System.Metrics


{-# INLINE buildWebLogger #-}
buildWebLogger
  ::( HasLogger env
    , HasMetrics env
    , HasWeb m cxt env
    , MonadIO n)
  => Factory n env env
buildWebLogger = do
  logFunc  <- asks (view askLogger)
  store    <- asks (view askMetrics)
  liftIO $ registerCounter "logger.failure" (logFail logFunc) store
  buildMiddleware $ \app req resH -> app req $ \res -> do
    toLog logFunc req (responseStatus res) Nothing
    resH res
