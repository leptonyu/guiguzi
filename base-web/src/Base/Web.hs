module Base.Web(
    module Base.Web.Types

  , module Base.Middleware.Actuator
  , module Base.Middleware.Trace

  , module Base.Health
  , module Base.Vault

  , buildWeb
  , HasSwagger
  ) where

import           Base.Client
import           Base.Health
import           Base.Metrics
import           Base.Middleware.Actuator
import           Base.Middleware.Consul
import           Base.Middleware.Error
import           Base.Middleware.Trace
import           Base.Web.Types

import           Base.Vault
import           Boots
import           Lens.Micro.Extras
import           Servant
import           Servant.Swagger

toWeb :: HasVault cxt cxt => Web IO cxt -> Web (App cxt) cxt
toWeb Web{..} = Web{ nature = \_ _ v -> runVault context v ,..}

buildWeb
  :: forall cxt n api
  . ( MonadIO n
    , MonadCatch n
    , HasLogger cxt
    , HasSalak cxt
    , HasApp cxt
    , HasVault cxt cxt
    , HasHttpClient cxt
    , HasHealth cxt
    , HasSwagger api
    , HasServer api '[cxt])
  => Proxy api
  -> ServerT api (App cxt)
  -> Factory n (Web (App cxt) cxt) (Web (App cxt) cxt)
  -> Factory n cxt (IO ())
buildWeb proxy server mid = do
  env <- ask
  wc  <- require "application"
  str <- liftIO newStore
  let proxycxt     = Proxy @cxt
      proxym       = Proxy @(App cxt)
      web0@Web{..} = toWeb (defWeb env str wc)
      AppEnv{..}   = view askApp env
  logInfo $ "Start Service [" <> name <> "] ..."
  polish web0
    [ mid
    , buildTrace          proxym proxycxt
    , buildActuators      proxym proxycxt
    , serveWebWithSwagger proxym proxycxt True proxy server
    , buildError          proxym proxycxt
    , buildConsul         proxym proxycxt
    ] >>> runWeb @(App cxt) @cxt
