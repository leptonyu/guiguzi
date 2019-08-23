module Base.Middleware.Logger where

import           Base.Metrics
import           Base.Web.Types
import           Boots
import           Control.Exception      (SomeException, try)
import           Lens.Micro.Extras
import           Network.Wai
import           System.Metrics
import qualified System.Metrics.Counter as Counter


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
  log_fail <- liftIO $ createCounter "logger.failure" store
  buildMiddleware $ \app req resH -> app req $ \res -> do
    v <- try $ toLog logFunc req (responseStatus res) Nothing
    case v of
      Left (_ :: SomeException) -> Counter.inc log_fail
      _                         -> return ()
    resH res
