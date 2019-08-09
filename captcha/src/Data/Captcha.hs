{-# LANGUAGE UndecidableInstances #-}
module Data.Captcha where

import Servant
import Servant.Server.Internal
import Servant.Swagger
import Base.Web
import Boots
import Network.Wai.Internal

data CheckCaptcha

instance (HasVault env env, HasContextEntry context env, HasServer api context) 
  => HasServer (CheckCaptcha :> api) context where
  type ServerT (CheckCaptcha :> api) m = ServerT api m
  route _ c s = route (Proxy @api) c $ s `addMethodCheck` runVaultInDelayedIO @env (getContextEntry c) go
    where
      go Request{..} = return ()
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger api => HasSwagger (CheckCaptcha :> api) where
  toSwagger _ = toSwagger (Proxy @api)