module Base.Actuator.Health where

import           Base.Actuator
import           Base.Health
import           Base.Web.Types
import           Boots
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Lens.Micro.Extras
import           Servant

type HealthEndpoint = "health" :> Get '[JSON] Health

actuatorHealth
  ::( HasSalak env
    , HasLogger env
    , HasHealth env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy m -> Proxy cxt -> ActuatorConfig -> Plugin env n env
actuatorHealth pm pc ac = asks (view askHealth)
  >>= newActuator pm pc ac "health" (Proxy @HealthEndpoint) . liftIO
