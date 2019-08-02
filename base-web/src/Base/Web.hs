module Base.Web(
    module Base.Web.Types

  , module Base.Middleware.Actuator
  , module Base.Middleware.Trace

  , module Base.Health

  , pluginWeb
  , HasSwagger
  ) where

import           Base.Web.Types

import           Base.Middleware.Actuator
import           Base.Middleware.Trace

import           Base.Health
import           Base.Metrics

import           Base.Dto
import           Base.Middleware.Error
import           Boots
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Lens.Micro
import           Lens.Micro.Extras
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
    , HasApp cxt
    , HasSwagger api
    , HasServer api '[cxt])
  => Proxy api
  -> ServerT api (App cxt)
  -> Plugin (Web (App cxt) cxt) n (Web (App cxt) cxt)
  -> Plugin cxt n (IO ())
pluginWeb proxy server mid = do
  env <- ask
  wc  <- require "application"
  str <- liftIO newStore
  let proxycxt     = Proxy @cxt
      proxym       = Proxy @(App cxt)
      web0@Web{..} = toWeb (defWeb env str wc)
      AppContext{..} = view askApp env
  logInfo $ "Start Service [" <> name <> "] ..."
  web <- promote web0 $ combine
    [ mid
    , pluginTrace         proxym proxycxt (local . over askLogger)
    , pluginActuators     proxym proxycxt
    , serveWebWithSwagger proxym proxycxt True proxy server
    , pluginError         proxym proxycxt
    ]
  promote web $ buildWeb @(App cxt) @cxt
