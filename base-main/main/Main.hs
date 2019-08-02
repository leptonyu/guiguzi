module Main where

import           Base.Env
import           Base.Main
import           Base.Web
import           Boots
import           Paths_base_main
import           Servant

type DemoAPI = "hello" :> Get '[PlainText] String

demoServer = demo

demo =  do
  logDebug "debug"
  logInfo  "info"
  logWarn  "warn"
  logError "error"
  return "Hello"

main = start version "guiguzi" (Proxy @DemoAPI) demoServer
