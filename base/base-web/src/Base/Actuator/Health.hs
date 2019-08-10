module Base.Actuator.Health where

import           Base.Actuator
import           Base.Health
import           Base.Web.Types
import           Boots
import           Control.Concurrent.MVar
import           Data.Aeson              (encode)
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
  => ActuatorConfig -> Factory n env env
actuatorHealth ac = do
  HealthRef ref <- asks (view askHealth)
  liftIO (readMVar ref)
    >>= newActuator ac "health" (Proxy @HealthEndpoint) . liftIO . go
  where
    go health = do
      h@Health{..} <- health
      if status == UP then return h else throwM err400 { errBody = encode h }

