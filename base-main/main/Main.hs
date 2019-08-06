module Main where

import           Base.Main
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

main = start Paths_base_main.version "guiguzi" (Proxy @DemoAPI) demoServer
