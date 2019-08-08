module Base.Main where

import           Base.Client
import           Base.Env
import           Base.Web
import           Boots
import           Data.Proxy
import           Data.Version
import           Servant
import           Unsafe.Coerce (unsafeCoerce)

start
  :: forall api db
  .( HasSwagger api, HasServer api '[MainEnv db])
  => Version
  -> String
  -> Factory IO (MainEnv EmptyDB) db
  -> Proxy api
  -> ServerT api (App (MainEnv db))
  -> IO ()
start ver appname fac proxy server = boot $ do
  app <- buildApp appname ver
  within app $ do
    client <- buildClient
    health <- liftIO emptyHealth
    dbn <- within MainEnv{db=EmptyDB, ..} fac
    within MainEnv{ db=dbn, app = unsafeCoerce app, ..}
      $ buildWeb proxy server ask
