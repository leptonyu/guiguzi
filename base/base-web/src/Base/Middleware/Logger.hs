module Base.Middleware.Logger where

import           Base.Web.Types
import           Boots
import           Lens.Micro.Extras
import           Network.Wai


{-# INLINE buildWebLogger #-}
buildWebLogger
  :: ( HasLogger env
     , HasWeb m cxt env)
  => Factory n env env
buildWebLogger = do
  logFunc    <- asks (view askLogger)
  buildMiddleware $ \app req resH -> app req $ \res -> do
    toLog logFunc req (responseStatus res) Nothing
    resH res
