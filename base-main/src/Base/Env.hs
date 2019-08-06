module Base.Env where

import           Base.Client
import           Base.Database
import           Base.Redis
import           Boots
import           Lens.Micro

data MainEnv = MainEnv
  { app      :: !AppEnv
  -- Client
  , client   :: !HttpClient
  -- Db
  , database :: DB
  , redis    :: REDIS
  }

class HasMainEnv env where
  askMainEnv :: Lens' env MainEnv
instance HasMainEnv MainEnv where
  askMainEnv = id
instance HasSalak MainEnv where
  askSourcePack = askApp . askSourcePack
instance HasLogger MainEnv where
  askLogger = askApp . askLogger
instance HasDataSource MainEnv where
  askDataSource = lens database (\x y -> x { database = y})
instance HasRedis MainEnv where
  askRedis = lens redis (\x y -> x { redis = y})
instance HasApp MainEnv where
  askApp = lens app (\x y -> x { app = y})
instance HasHttpClient MainEnv where
  askHttpClient = lens client (\x y -> x { client = y})
