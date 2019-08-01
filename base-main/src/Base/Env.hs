module Base.Env where

import           Base.Client
import           Base.Database
import           Base.Dto
import           Base.Redis
import           Boots.Plugin.Logger
import           Boots.Plugin.Salak
import           Lens.Micro
import           Salak

data MainEnv = MainEnv
  { app        :: AppContext

  -- Simple
  , sourcePack :: SourcePack
  , logFunc    :: LogFunc

  -- Client
  , client     :: HttpClient

  -- Db
  , database   :: DB
  , redis      :: REDIS
  }

class HasMainEnv env where
  askMainEnv :: Lens' env MainEnv
instance HasMainEnv MainEnv where
  askMainEnv = id
instance HasSalak MainEnv where
  askSourcePack = lens sourcePack (\x y -> x { sourcePack = y})
instance HasLogger MainEnv where
  askLogger = lens logFunc (\x y -> x { logFunc = y})
instance HasDataSource MainEnv where
  askDataSource = lens database (\x y -> x { database = y})
instance HasRedis MainEnv where
  askRedis = lens redis (\x y -> x { redis = y})
instance HasApp MainEnv where
  askApp = lens app (\x y -> x { app = y})
