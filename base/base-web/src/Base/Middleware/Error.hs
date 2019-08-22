module Base.Middleware.Error where

import           Base.Web.Types
import           Boots
import           Control.Exception (catch)
import           Lens.Micro.Extras
import           Network.Wai       (vault)

{-# INLINE buildError #-}
buildError
  :: (HasWeb m cxt env, HasVault cxt cxt, MonadIO n, HasLogger cxt)
  => Factory n env env
buildError = do
  Web{..} <- asks (view askWeb)
  buildMiddleware
    $ \app req resH -> app req resH `catch`
      \e -> do
        logException context (vault req) e
        resH (whenException e)
