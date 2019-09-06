{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Server.Captcha where

import           API.Captcha
import           Base.Redis
import           Boots
import           Boots.Web
import           Data.ByteString          (ByteString)
import           Data.Swagger             hiding (HeaderName)
import qualified Database.Redis           as R
import           Lens.Micro
import           Network.HTTP.Types
import           Network.Wai.Internal
import           Servant
import           Servant.Server.Internal
import           Servant.Swagger
import           Servant.Swagger.Internal (addParam)
import Data.Text.Encoding

{-# INLINE hCaptcha #-}
hCaptcha :: HeaderName
hCaptcha = "X-CAPTCHA"

instance (HasRedis env, HasWeb context env, HasServer api context) => HasServer (CheckCaptcha :> api) context where
  type ServerT (CheckCaptcha :> api) m = ServerT api m
  route _ c s = route (Proxy @api) c $ s `addMethodCheck` runAppT c (lift ask >>= go)
    where
      go :: Request -> AppT (Context context) DelayedIO ()
      go Request{..} = case lookup hCaptcha requestHeaders of
        Just captcha -> checkCaptcha captcha
        _            -> throwM err401 { errBody = "Captcha invalid"}
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

checkCaptcha :: (HasRedis env, HasWeb context env, MonadIO m, MonadThrow m) => ByteString -> AppT (Context context) m ()
checkCaptcha captcha = do
  v <- R.liftRedis $ R.del ["c:" <> captcha]
  case v of
    Left  r -> logError (fromString $ show r) >> throwM err401 { errBody = "Captcha invalid" }
    Right i -> when (i == 0) $ throwM err401 { errBody = "Captcha invalid" }

captchaServer :: ServerT CaptchaAPI Handler
captchaServer = liftIO go :<|> return NoContent
  where
    go = do
      (code, b) <- newCaptcha
      let body = encodeUtf8 b
          cid  = fromString code
      -- _ <- R.liftRedis $ R.setex ("c:" <> encodeUtf8 cid <> ":" <> fromString code) 300 "true"
      return Captcha{..}

instance ToSchema Captcha

instance HasSwagger api => HasSwagger (CheckCaptcha :> api) where
  toSwagger _ = toSwagger (Proxy @api)
    & addParam param
    where
      {-# INLINE param #-}
      param = mempty
        & Data.Swagger.name .~ "X-CAPTCHA"
        & required ?~ True
        & schema .~ ParamOther (mempty
            & in_ .~ ParamHeader)


