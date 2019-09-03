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

captchaServer = liftIO go :<|> return NoContent
  where
    go = do
      (c, b) <- newCaptcha
      let body = decodeUtf8 b
          cid  = fromString (show c)
      return Captcha{..}

main :: IO ()
main = bootWebEnv "captcha-server" Paths_captcha_server.version (return ()) $ do
  tryServeWithSwagger True Proxy (Proxy @CaptchaAPI) captchaServer
