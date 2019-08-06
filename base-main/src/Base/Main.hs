module Base.Main where

import           Base.Client
import           Base.Env
import           Base.Web
import           Boots
import           Data.Proxy
import           Data.Version
import           Servant

start
  :: forall api db
  .( HasSwagger api, HasServer api '[MainEnv db])
  => Version
  -> String
  -> Factory IO (MainEnv EmptyDB) (MainEnv db)
  -> Proxy api
  -> ServerT api (App (MainEnv db))
  -> IO ()
start ver appname fac proxy server = boot $ do
  app <- buildApp appname ver
  within app $ do
    client <- buildClient
    let
      health = emptyHealth
      db = EmptyDB
    within MainEnv{..} fac >>> buildWeb proxy server ask
