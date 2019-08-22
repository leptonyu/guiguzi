{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Base.Env
import           Base.Main
import           Base.Redis
import           Base.Database
import           Boots
import           Data.Captcha
import           Lens.Micro
import           Paths_main
import           Servant

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

type DemoAPI =
  CaptchaEndpoint
  :<|> CheckCaptcha :> "hello" :> Get '[PlainText] String
  :<|>                "hellox" :> Get '[PlainText] String

demoServer = captchaServer :<|> demo :<|> return "Hello"

demo :: AppE String
demo =  do
  logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  logError "error"
  return "Hello"

main = start Paths_main.version "guiguzi" go (Proxy @DemoAPI) demoServer
  where
    go = do
      redis    <- buildRedis
      database <- buildDatabase
      return PDB{..}

