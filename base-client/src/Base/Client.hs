module Base.Client where

import           Boots
import           Data.Default
import           Lens.Micro
import           Network.HTTP.Client hiding (Proxy)
import           Salak

instance Default ManagerSettings where
  def = defaultManagerSettings

instance Monad m => FromProp m ResponseTimeout where
  fromProp = responseTimeoutMicro <$> fromProp

instance Monad m => FromProp m ManagerSettings where
  fromProp = do
    connCount <- "max-conns"  .?: managerConnCount
    timeout   <- "timeout"    .?: managerResponseTimeout
    idleCount <- "idle-conns" .?: managerIdleConnectionCount
    return def
      { managerConnCount           = connCount
      , managerResponseTimeout     = timeout
      , managerIdleConnectionCount = idleCount
      }

newtype HttpClient = HttpClient ManagerSettings

buildClient :: (HasSalak env, MonadIO m, MonadThrow m) => Factory m env HttpClient
buildClient = HttpClient <$> require "client"

class HasHttpClient env where
  askHttpClient :: Lens' env HttpClient

instance HasHttpClient HttpClient where
  askHttpClient = id
