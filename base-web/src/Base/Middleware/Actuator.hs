{-# LANGUAGE PolyKinds #-}
module Base.Middleware.Actuator where

import           Base.Web.Types
import           Boots
import           Control.Monad
import           Data.Maybe
import           Data.Proxy
import           Data.Text       (Text)
import           Salak
import           Servant
import           Servant.Swagger

newtype ActuatorConfig = ActuatorConfig
  { enabled   :: Bool }

instance Monad m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled" .?= True

class Actuator m cxt env tp where
  actuator :: (MonadIO n, MonadThrow n) => Proxy m -> Proxy cxt -> Proxy tp -> ActuatorConfig -> Plugin env n env

data EmptyActuator

instance Actuator m cxt env EmptyActuator where
  actuator _ _ _ _ = ask

emptyActuator :: Proxy EmptyActuator
emptyActuator = Proxy

instance (Actuator m cxt env a, Actuator m cxt env b) => Actuator m cxt env (a :<|> b) where
  actuator pn p _ ac = actuator pn p (Proxy @a) ac `union` actuator pn p (Proxy @b) ac

mkActuator
  :: forall m cxt env (api :: *) n
  . ( HasServer api '[cxt]
    , HasSwagger api
    , HasSalak env
    , HasLogger env
    , HasWeb m cxt env
    , MonadThrow n
    , MonadIO n)
  => Proxy m
  -> Proxy cxt
  -> Text
  -> ActuatorConfig
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
mkActuator pm pc name ActuatorConfig{..} _ s = do
  b <- require $ "actuator." <> name <> ".enabled"
  let ok = enabled && fromMaybe True b
  when ok $ logInfo $ "Load actuator " <> name <> "."
  serveWebWithSwagger pm pc ok
    (Proxy @(SwaggerTag "actuator" "Actuator Endpoint" :> "actuator" :> api)) s















