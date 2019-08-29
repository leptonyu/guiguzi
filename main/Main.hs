{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Base.Redis
import           Boots
import           Boots.Cloud
import           Boots.Web
import           Data.Captcha
import           Paths_main
import           Servant

type AppE = App (Env REDIS)

type API =
  CaptchaEndpoint
  :<|> "hello" :> Get '[PlainText] String
  :<|> "hellox" :> Get '[PlainText] String

apiServer = captchaServer :<|> demo :<|> return "Hello"

demo :: AppE String
demo =  do
  -- logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  -- logError "error"
  return "Hello"

main = bootWebEnv "main" Paths_main.version buildRedis $ do
  buildConsul
  tryServeWithSwagger True Proxy (Proxy @API) apiServer


