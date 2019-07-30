module Base.Middleware.Trace where

import           Base.Web.Config
import           Base.Web.Middleware
import           Boots
import           Control.Monad.IO.Class
import qualified Data.Vault.Lazy        as L
import           Lens.Micro
import           Network.Wai

pluginTrace
  :: forall cxt env m
  . (HasNatureT cxt env, HasLogger cxt, HasMiddleware env, MonadIO m, HasLogger env)
  => Plugin env m env
pluginTrace = do
  logInfo "Load plugin trace."
  env <- ask
  key <- liftIO L.newKey
  promote (over askNT (\(NatureT f) -> NatureT $ \v (cxt :: cxt) -> f v (g (L.lookup key v) cxt)) env)
    $ middlewarePlugin $ \app req -> app req {vault = L.insert key "hello" $ vault req}
  where
    g Nothing  cxt = cxt
    g (Just s) cxt = over askLogger (addTrace s) cxt
