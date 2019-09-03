{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Base.Redis
import           Boots
import           Boots.Cloud
import           Boots.Web
import           Data.Captcha
import           Paths_main
import           Servant

type AppE = App (AppEnv ABC)


data ABC = ABC
  { redis :: REDIS
  , hello :: ()
  }

instance HasRedis ABC where
  askRedis = lens redis (\x y -> x {redis = y})

type API =
  CaptchaEndpoint
  :<|> "hello" :> Get '[PlainText] String
  :<|> CheckCaptcha :> "hellox" :> Get '[PlainText] String

apiServer = captchaServer :<|> demo :<|> return "Hello"

demo :: AppE String
demo =  do
  -- logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  -- logError "error"
  return "Hello"

main = bootWebEnv "main" Paths_main.version go $ do
  buildConsul
  tryServeWithSwagger True Proxy (Proxy @API) apiServer
  where
    go = do
      redis <- buildRedis
      let hello = ()
      return ABC{..}


