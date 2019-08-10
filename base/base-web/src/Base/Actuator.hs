module Base.Actuator where

import           Base.Web.Swagger
import           Base.Web.Types
import           Boots
import           Control.Monad
import           Data.Maybe
import           Data.Proxy
import           Data.Text        (Text)
import           Salak
import           Servant
import           Servant.Swagger

newtype ActuatorConfig = ActuatorConfig { enabled :: Bool }

instance Monad m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled" .?= True

newActuator
  :: forall m cxt api env n
  . ( HasServer api '[cxt]
    , HasSwagger api
    , HasSalak env
    , HasLogger env
    , HasWeb m cxt env
    , MonadIO n
    , MonadThrow n)
  => ActuatorConfig -> Text -> Proxy api -> ServerT api m -> Factory n env env
newActuator ActuatorConfig{..} name _ s = do
  b <- require $ "actuator." <> name <> ".enabled"
  let ok = enabled && fromMaybe True b
  when ok $ logInfo $ "Load actuator " <> name <> "."
  serveWebWithSwagger ok (Proxy @(SwaggerTag "actuator" "Actuator Endpoint" :> "actuator" :> api)) s


