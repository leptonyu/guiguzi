module Base.Redis where

import           Boots
import           Control.Exception (Exception, throw)
import           Data.Maybe
import           Data.Word
import           Database.Redis
import           Salak

instance Default ConnectInfo where
  def = defaultConnectInfo

instance FromProp m ConnectInfo where
  fromProp = ConnInfo
    <$> "host"      .?: connectHost
    <*> "port"      .?: connectPort
    <*> "password"  .?: connectAuth
    <*> "database"  .?: connectDatabase
    <*> "max-conns" .?: connectMaxConnections
    <*> "max-idle"  .?: connectMaxIdleTime
    <*> return Nothing
    <*> return Nothing

instance FromProp m PortID where
  fromProp = PortNumber . fromIntegral <$> (fromProp :: Prop m Word16)

-- | Middleware context type.
newtype REDIS = REDIS Connection

data RedisException
  = RedisException !String
  | RedisNotInitializedException
  | RedisAbortedException
  deriving Show

instance Exception RedisException

class HasRedis env where
  askRedis :: Lens' env REDIS

instance HasRedis REDIS where
  askRedis = id

instance HasRedis ext => HasRedis (Env ext) where
  askRedis = askExt . askRedis

instance (HasRedis env, MonadIO m) => MonadRedis (AppT env m) where
  liftRedis ra = do
    REDIS c <- asks (view askRedis)
    liftIO $ runRedis c ra

check :: REDIS -> IO HealthStatus
check (REDIS c) = runRedis c ping >>= go
  where
    go (Left e) = throw $ RedisException $ show e
    go _        = return UP

buildRedis
  :: (MonadMask m, MonadIO m, HasSalak env, HasLogger env, HasHealth env)
  => Factory m env REDIS
buildRedis = do
  enabled <- fromMaybe True <$> require "redis.enabled"
  if enabled
    then do
      ci    <- require "redis"
      logInfo "Load redis"
      rd    <- REDIS <$> produce (liftIO $ connect ci) (liftIO . disconnect)
      registerHealth "redis" (check rd)
      return rd
    else do
      logInfo "Disable redis module"
      return (throw RedisNotInitializedException)
