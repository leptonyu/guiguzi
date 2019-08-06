module Base.Redis where

import           Base.Health
import           Boots
import           Control.Exception (Exception, throw)
import           Data.Default
import           Data.Maybe
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

data RedisException
  = RedisException !String
  | RedisNotInitializedException
  deriving Show

instance Exception RedisException

class HasRedis env where
  askRedis :: Lens' env REDIS

instance HasRedis REDIS where
  askRedis = id

check :: REDIS -> IO HealthStatus
check (REDIS c) = runRedis c ping >>= go
  where
    go (Left e) = throw $ RedisException $ show e
    go _        = return UP

buildRedis
  :: (MonadCatch m, MonadIO m, HasSalak env, HasLogger env, HasHealth env)
  => Factory m env (env, REDIS)
buildRedis = do
  env     <- ask
  enabled <- fromMaybe True <$> require "redis.enabled"
  if enabled
    then do
      ci    <- require "redis"
      logInfo "Redis loading"
      rd    <- REDIS <$> bracket (liftIO $ connect ci) (liftIO . disconnect)
      env1  <- within env $ buildHealth ("redis", check rd)
      return (env1, rd)
    else return (env, throw RedisNotInitializedException)
