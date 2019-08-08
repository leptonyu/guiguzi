{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Base.Database
import           Base.Env
import           Base.Main
import           Base.Redis
import           Base.Web
import           Boots
import           Lens.Micro
import           Paths_base_main
import           Servant
import           Servant.Server.Internal
import           Servant.Swagger

data PDB = PDB
  { redis    :: REDIS
  , database :: DB
  }

type Env = MainEnv PDB

type AppE = App Env

instance HasRedis Env where
  askRedis = askDb . lens redis (\x y -> x {redis = y})
instance HasDataSource Env where
  askDataSource = askDb . lens database (\x y -> x {database = y})

type DemoAPI = Log :> "hello" :> Get '[PlainText] String

data Log

instance (HasVault context context, HasLogger context, HasServer api '[context]) => HasServer (Log :> api) '[context] where
  type ServerT (Log :> api) m = ServerT api m
  route _ c@(cxt:. EmptyContext) Delayed{..} = route (Proxy @api) c Delayed{methodD = methodD >> go, ..}
    where
      go = runVaultInDelayedIO cxt $ logInfo "Hello"
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger api => HasSwagger (Log :> api) where
  toSwagger _ = toSwagger (Proxy @api)

demoServer = demo

demo :: AppE String
demo =  do
  logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  logError "error"
  return "Hello"

main = start Paths_base_main.version "guiguzi" go (Proxy @DemoAPI) demoServer
  where
    go = do
      redis       <- buildRedis
      database    <- buildDatabase
      return PDB{..}

