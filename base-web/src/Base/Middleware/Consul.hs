module Base.Middleware.Consul where

import           Base.Client
import           Base.Dto
import           Base.Web.Types
import           Boots
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Proxy
import           Data.Version           (showVersion)
import           Lens.Micro.Extras
import           Network.Consul

pluginConsulClient
  :: forall m cxt env n
  . ( HasApp env
    , HasLogger env
    , HasSalak env
    , HasWeb m cxt env
    , HasHttpClient cxt
    , MonadIO n
    , MonadCatch n)
  => Proxy m -> Proxy cxt -> Plugin env n env
pluginConsulClient _ _ = do
  b  <- fromMaybe True <$> require "consul.enabled"
  if b
    then do
      env <- ask
      let AppContext{..} = view askApp env
          Web{..}        = view askWeb env :: Web m cxt
          hc             = view askHttpClient context
          WebConfig{..}  = config
      consul        <- require "consul"
      ConsulApi{..} <- promote (consul :: ConsulConfig) $
        return (askConsul @ConsulConfig @IO hc)
      let
        open = liftIO
          $ runAppT consul
          $ registerService
          $ newServer
          $ HttpServer name inst (Just hostname) (Just port) [ "version:" <> showVersion ver ]
          $ ServiceCheck name inst "" ""
          $ "http://" <> hostname <> ":" <> show port <>"/actuator/health"
        close _ = void
          $ liftIO
          $ runAppT consul
          $ deregisterService inst
      delay $ logInfo "Service deregistered from consul."
      _ <- bracketP open close
      logInfo "Service registered to consul."
      ask
    else ask

