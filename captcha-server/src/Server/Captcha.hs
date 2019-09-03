{-# LANGUAGE TypeFamilies #-}
module Server.Captcha where

import           API.Captcha
import           Boots
import           Data.Swagger             hiding (HeaderName)
import           Lens.Micro
import           Network.HTTP.Types
import           Network.Wai.Internal
import           Servant
import           Servant.Server.Internal
import           Servant.Swagger
import           Servant.Swagger.Internal (addParam)


{-# INLINE hCaptcha #-}
hCaptcha :: HeaderName
hCaptcha = "X-CAPTCHA"

instance HasServer api context => HasServer (CheckCaptcha :> api) context where
  type ServerT (CheckCaptcha :> api) m = ServerT api m
  route _ c s = route (Proxy @api) c $ s `addMethodCheck` runAppT c (lift ask >>= go)
    where
      go :: Request -> AppT (Context context) DelayedIO ()
      go Request{..} = case lookup hCaptcha requestHeaders of
        Just captcha -> checkCaptcha captcha
        _            -> throwM err401 { errBody = "Captcha invalid"}
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

checkCaptcha = undefined

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


