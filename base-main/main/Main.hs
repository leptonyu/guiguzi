module Main where

import           Base.Main
import           Boots
import           Paths_base_main
import           Servant

type DemoAPI = "hello" :> Get '[PlainText] String

main = start version "guiguzi" (Proxy @DemoAPI) (logInfo "hello" >> return "Hello")
