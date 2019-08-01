module Base.Web(
    module Base.Web.Types

  , module Base.Middleware.Actuator
  , module Base.Middleware.Trace

  , module Base.Health
  , module Base.Actuator.Health
  , module Base.Actuator.Refresh

  , pluginWeb
  , HasSwagger
  ) where

import           Base.Web.Types

import           Base.Middleware.Actuator
import           Base.Middleware.Trace

import           Base.Actuator.Health
import           Base.Actuator.Refresh
import           Base.Health

import           Boots
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Version             (Version)
import           Lens.Micro
import           Servant
import           Servant.Swagger

toWeb :: Web Servant.Handler cxt -> Web (App cxt) cxt
toWeb Web{..} = Web{ nature = \pc _ v ma -> nature pc (Proxy @Servant.Handler) v $ liftIO $ runAppT context ma ,..}

pluginWeb
  :: forall cxt n api
  . ( MonadIO n
    , MonadThrow n
    , HasLogger cxt
    , HasSalak cxt
    , HasSwagger api
    , HasServer api '[cxt])
  => Version
  -> Proxy api
  -> ServerT api (App cxt)
  -> Plugin (Web (App cxt) cxt) n (Web (App cxt) cxt)
  -> Plugin cxt n (IO ())
pluginWeb v proxy server mid = do
  env <- ask
  let proxycxt     = Proxy @cxt
      proxym       = Proxy @(App cxt)
      web0@Web{..} = toWeb (defWeb env)
  ac  <- require "actuator"
  logInfo $ "Start Service [" <> name config <> "] ..."
  web <- promote web0 $ combine
    [ mid
    , pluginTrace proxym proxycxt (local . over askLogger)
    , actuator proxym proxycxt (Proxy @(Health :<|> Refresh)) ac
    , serveWebWithSwagger proxym proxycxt True proxy server
    ]
  promote web $ buildWeb @(App cxt) @cxt v
