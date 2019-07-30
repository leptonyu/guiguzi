{-# LANGUAGE PolyKinds #-}
module Base.Web.Actuator where

import           Base.Web.Serve
import           Base.Web.Swagger
import           Boots
import           Control.Monad
import           Data.Maybe
import           Data.Proxy
import           Data.Text        (Text)
import           Salak
import           Servant
import           Servant.Swagger

newtype ActuatorConfig = ActuatorConfig
  { enabled   :: Bool }

instance Monad m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled" .?= True

class Actuator (cxt :: [k]) env tp where
  actuator :: (MonadIO m, MonadThrow m) => Proxy cxt -> Proxy tp -> ActuatorConfig -> Plugin env m env

data EmptyActuator

instance Actuator cxt env EmptyActuator where
  actuator _ _ _ = ask

emptyActuator :: Proxy EmptyActuator
emptyActuator = Proxy

instance (Actuator cxt env a, Actuator cxt env b) => Actuator cxt env (a :<|> b) where
  actuator p _ ac = actuator p (Proxy @a) ac >>= (`promote` actuator p (Proxy @b) ac)

mkActuator
  :: forall m cxt env (api :: *)
  . ( HasServer api cxt
    , HasSwagger api
    , HasSalak env
    , HasSwaggerProxy env
    , HasServeWeb cxt env
    , MonadThrow m
    , MonadIO m
    , HasLogger env)
  => Text
  -> ActuatorConfig
  -> Proxy cxt
  -> Proxy api
  -> Server api
  -> Plugin env m env
mkActuator name ActuatorConfig{..} p _ s = do
  b <- require $ "actuator." <> name <> ".enabled"
  let ok = enabled && fromMaybe True b
  when ok $ logInfo $ "Load actuator " <> name <> "."
  tryServePlugin (const id) ok p
    (Proxy @(SwaggerTag "actuator" "Actuator Endpoint" :> "actuator" :> api))
    s














