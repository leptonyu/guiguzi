module Base.Web(
    module Base.Web.Types
  , module Base.Web.Swagger

  , module Base.Middleware.Actuator
  , module Base.Middleware.Trace

  , module Base.Health

  , buildWeb
  , runVaultInDelayedIO
  , HasSwagger
  ) where

import           Base.Client
import           Base.Health
import           Base.Metrics
import           Base.Middleware.Actuator
import           Base.Middleware.Consul
import           Base.Middleware.Error
import           Base.Middleware.Logger
import           Base.Middleware.SplitRandom
import           Base.Middleware.Trace
import           Base.Web.Swagger
import           Base.Web.Types

import           Boots
import           Lens.Micro.Extras
import           Network.Wai
import           Servant
import           Servant.Server.Internal
import           Servant.Swagger

{-# INLINE toWeb #-}
toWeb :: (HasLogger cxt, HasVault cxt cxt) => Web IO cxt -> Web (App cxt) cxt
toWeb Web{..} = Web{ nature = runVault context, ..}

{-# INLINE runVaultInDelayedIO #-}
runVaultInDelayedIO :: HasVault context context => context -> (Request -> AppT context DelayedIO a) -> DelayedIO a
runVaultInDelayedIO c ma = do
  req <- DelayedIO ask
  runVault c (vault req) (ma req)

buildWeb
  ::( MonadIO n
    , MonadMask n
    , HasLogger cxt
    , HasSalak cxt
    , HasApp cxt cxt
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
  let web0@Web{..} = toWeb (defWeb env str wc)
      AppEnv{..}   = view askApp env
  logInfo $ "Start Service [" <> name <> "] ..."
  polish web0
    [ mid
    , buildActuators
    , buildError
    , serveWebWithSwagger True proxy server
    , buildConsul
    , buildWebLogger
    , buildTrace
    , buildSplitRandom
    ] >>> runWeb
