module Main where

import           Base.Env
import           Base.Main
import           Base.Web
import           Boots
import           Network.Consul
import           Paths_base_main
import           Servant

type DemoAPI = "hello" :> Get '[PlainText] String :<|> (SwaggerTag "consul" "Consul Endpoint" :> ConsulEndpoint)

demoServer = demo :<|> consulServer (Proxy @IO) (Proxy @MainEnv)


demo =  do
  logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  logError "error"
  return "Hello"

main = start version "guiguzi" (Proxy @DemoAPI) demoServer
