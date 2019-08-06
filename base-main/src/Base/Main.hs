module Base.Main where

import           Base.Client
import           Base.Database
import           Base.Env
import           Base.Redis
import           Base.Web
import           Boots
import           Data.Proxy
import           Data.Version
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
      $ buildHealth [dbHealth, rdHealth]
