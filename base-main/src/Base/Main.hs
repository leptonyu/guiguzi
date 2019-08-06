module Base.Main where

import           Base.Client
import           Base.Database
import           Base.Env
import           Base.Redis
import           Base.Web
import           Boots
import           Data.Maybe
import           Data.Proxy
import           Data.Version
import           Lens.Micro
import           Servant

start
  :: forall api
  .( HasSwagger api, HasServer api '[MainEnv])
  => Version
  -> String
  -> Proxy api
  -> ServerT api (App MainEnv)
  -> IO ()
start ver appname proxy server = boot $ do
  app <- buildApp appname ver
  within app $ do
    client               <- buildClient
    (database, dbHealth) <- buildDatabase
    (redis,    rdHealth) <- buildRedis
    within MainEnv{..}
      $ buildWeb proxy server
      $ asks
      $ over (askWeb @(App MainEnv) @MainEnv . askHealth)
      $ combineHealth
      $ catMaybes [dbHealth, rdHealth]

