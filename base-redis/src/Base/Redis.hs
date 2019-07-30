module Base.Redis where

import           Boots
import           Data.Default
import           Data.Word
import           Database.Redis
import           Lens.Micro
import           Salak

instance Default ConnectInfo where
  def = defaultConnectInfo

instance Monad m => FromProp m ConnectInfo where
  fromProp = ConnInfo
    <$> "host"      .?: connectHost
    <*> "port"      .?: connectPort
    <*> "password"  .?: connectAuth
    <*> "database"  .?: connectDatabase
    <*> "max-conns" .?: connectMaxConnections
    <*> "max-idle"  .?: connectMaxIdleTime
    <*> return Nothing
    <*> return Nothing

instance Monad m => FromProp m PortID where
  fromProp = PortNumber . fromIntegral <$> (fromProp :: Prop m Word16)

-- | Middleware context type.
newtype REDIS = REDIS Connection

class HasRedis env where
  askRedis :: Lens' env REDIS

instance HasRedis REDIS where
  askRedis = id

pluginRedis :: (MonadCatch m, MonadIO m, HasSalak env, HasLogger env) => Plugin env m REDIS
pluginRedis = do
  ci    <- require "redis"
  logInfo "Redis loading"
  REDIS <$> bracketP (liftIO $ connect ci) (liftIO . disconnect)
