module Main where

import           Base.Database
import           Base.Env
import           Base.Main
import           Base.Redis
import           Boots
import           Lens.Micro
import           Paths_base_main
import           Servant

data PDB = PDB
  { redis    :: REDIS
  , database :: DB
  }
type Env = MainEnv PDB
instance HasRedis Env where
  askRedis = askDb . lens redis (\x y -> x {redis = y})
instance HasDataSource Env where
  askDataSource = askDb . lens database (\x y -> x {database = y})

type DemoAPI = "hello" :> Get '[PlainText] String

demoServer = demo

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
      MainEnv{..} <- ask
      return MainEnv{db=PDB{..},..}

