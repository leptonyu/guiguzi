module Base.Middleware.Consul where

import           Base.Client
import           Base.Web.Types
import           Boots
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.String
import           Data.Version        (showVersion)
import           Lens.Micro.Extras
import           Network.Consul

buildConsul
  :: ( HasApp cxt env
    , HasLogger env
    , HasSalak env
    , HasWeb m cxt env
    , HasHttpClient cxt
    , MonadIO n
    , MonadCatch n)
  => Factory n env env
buildConsul = do
  b  <- fromMaybe False <$> require "consul.discovery.enabled"
  unless   b $ logInfo "Disable consul discovery module"
  tryBuild b $ do
    env <- ask
    let AppEnv{..}    = view askApp env
        Web{..}       = view askWeb env
        hc            = view askHttpClient context
        WebConfig{..} = config
    consul@ConsulConfig{..} <- require "consul"
    ConsulApi{..} <- within (consul :: ConsulConfig) $
      return (askConsul @ConsulConfig @IO hc)
    let
      met2 = HM.insert "version" (fromString $ showVersion version) meta
      open = liftIO
        $ runAppT consul
        $ registerService
        $ newServer
        $ HttpServer name instanceId (Just hostname) (Just port) tags met2
        $ ServiceCheck name instanceId interval dcsa
        $ "http://" <> hostname <> ":" <> show port <>"/actuator/health"
      close _ = void
        $ liftIO
        $ runAppT consul
        $ deregisterService instanceId
    delayA $ logInfo "Service deregistered from consul."
    _ <- bracket open close
    logInfo "Service registered to consul."
    return env

