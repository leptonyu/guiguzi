{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module API.Captcha(
    CaptchaAPI
  , CheckCaptcha
  , Captcha(..)
  ) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant.API

data CheckCaptcha

data Captcha = Captcha
  { cid  :: Text
  , body :: Text
  } deriving Generic

instance ToJSON Captcha where
  toJSON Captcha{..} = object [ "cid" .= cid, "body" .= body ]

type CaptchaAPI = "captcha" :> (Post '[JSON] Captcha :<|> CheckCaptcha :> Get '[JSON] NoContent)



