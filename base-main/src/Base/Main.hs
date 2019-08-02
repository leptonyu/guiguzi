module Base.Main where

import           Base.Client
import           Base.Database
import           Base.Dto
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
import           System.Random.MWC

start
  :: forall api
  .( HasSwagger api, HasServer api '[MainEnv])
  => Version
  -> String
  -> Proxy api
  -> ServerT api (App MainEnv)
  -> IO ()
start ver appname proxy server = boot $ do
  sourcePack <- pluginSalak appname
  promote sourcePack $ do
    name     <- fromMaybe (fromString appname) <$> require "application.name"
    genr     <- liftIO createSystemRandom
    inst     <- liftIO $ random64 genr
    logFunc  <- pluginLogger (name <> "," <> inst)
    promote Simple{..} $ do
      client               <- pluginClient
      (database, dbHealth) <- pluginDatabase
      (redis,    rdHealth) <- pluginRedis
      let app = AppContext{..}
      promote MainEnv{..}
        $ pluginWeb proxy server
        $ asks
        $ over (askWeb @(App MainEnv) @MainEnv . askHealth)
        $ combineHealth
        $ catMaybes [dbHealth, rdHealth]

