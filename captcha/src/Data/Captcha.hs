module Data.Captcha where

import Servant
import Servant.Server.Internal
import Servant.Swagger

data CheckCaptcha

instance HasServer api context => HasServer (CheckCaptcha :> api) context where
  type ServerT (CheckCaptcha :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger api => HasSwagger (CheckCaptcha :> api) where
  toSwagger _ = toSwagger (Proxy @api)