module Base.Actuator.Health where

import           Base.Health
import           Base.Middleware.Actuator
import           Base.Web.Types
import           Boots
import           Control.Monad.IO.Class
import           Lens.Micro.Extras
import           Servant


type HealthEndpoint = "health" :> Get '[JSON] Health

instance
  ( HasWeb m cxt env
  , HasSalak env
  , HasLogger env
  , MonadIO m) => Actuator m cxt env Health where
  actuator pm pc _ ac = do
    env     <- ask
    mkActuator pm pc "health" ac (Proxy @HealthEndpoint) (liftIO (view (askWeb @m @cxt . askHealth) env))
