{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           API.Captcha
import           Boots
import           Boots.Web
import           Control.Monad.IO.Class
import           Data.Captcha
import           Data.Text.Encoding
import           Paths_captcha_server
import           Servant
import           Server.Captcha         ()

main :: IO ()
main = bootWebEnv "captcha-server" Paths_captcha_server.version (return ()) $ do
  tryServeWithSwagger True Proxy (Proxy @CaptchaAPI) captchaServer
