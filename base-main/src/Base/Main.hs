module Base.Main where

import           Base.Client
import           Base.Database
import           Base.Env
import           Base.Redis
import           Base.Web
import           Boots
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Proxy
import           Data.String
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
start v appname proxy server = boot $ do
  sourcePack <- pluginSalak appname
  promote sourcePack $ do
    config   <- require (fromString appname)
    logFunc  <- pluginLogger (name config)
    promote Simple{..} $ do
      client               <- pluginClient
      (database, dbHealth) <- pluginDatabase
      (redis,    rdHealth) <- pluginRedis
      promote MainEnv{..}
        $ pluginWeb v proxy server
        $ asks
        $ over (askWeb @(App MainEnv) @MainEnv . askHealth)
        $ combineHealth
        $ catMaybes [dbHealth, rdHealth]

