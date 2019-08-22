{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Captcha where

import           Base.Redis
import           Base.Web
import           Boots                    hiding (name, version)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Captcha.Internal
import           Data.String
import           Data.Swagger             hiding (HeaderName)
import           Data.Swagger.Schema      (ToSchema)
import           Data.Text                (Text)
import           Data.Text.Encoding
import qualified Database.Redis           as R
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.HTTP.Types
import           Network.Wai.Internal
import           Servant
import           Servant.Server.Internal
import           Servant.Swagger
import           Servant.Swagger.Internal (addParam)

data CheckCaptcha

{-# INLINE hCaptcha #-}
hCaptcha :: HeaderName
hCaptcha = "X-CAPTCHA"

instance (HasLogger context, HasRedis context, HasVault context context, HasServer api '[context])
  => HasServer (CheckCaptcha :> api) '[context] where
  type ServerT (CheckCaptcha :> api) m = ServerT api m
  route _ c s = route (Proxy @api) c $ s `addMethodCheck` runVaultInDelayedIO @context (getContextEntry c) go
    where
      go Request{..} = case lookup hCaptcha requestHeaders of
        Just captcha -> checkCaptcha captcha
        _            -> throwM err401 { errBody = "Captcha invalid"}
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

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

data Captcha = Captcha
  { cid  :: String
  , body :: Text
  } deriving Generic

instance ToSchema Captcha

instance ToJSON Captcha

type CaptchaEndpoint = SwaggerTag "captcha" "Captcha Endpoint" :>"captcha" :> Post '[JSON] Captcha

checkCaptcha :: (HasLogger context, HasRedis context, MonadIO m, MonadThrow m) => B.ByteString -> AppT context m ()
checkCaptcha captcha = do
  v <- R.liftRedis $ R.del ["c:" <> captcha]
  case v of
    Left  r -> logError (fromString $ show r) >> throwM err401 { errBody = "Captcha invalid" }
    Right i -> when (i == 0) $ throwM err401 { errBody = "Captcha invalid" }

captchaServer :: forall env. (HasApp env env, HasRedis env) => App env Captcha
captchaServer = do
  (AppEnv{..} :: AppEnv env) <- asks (view askApp)
  cid           <- rand64 randSeed
  (code, body1) <- liftIO $ newCaptcha font
  let body  = ("image/png;base64 " <>) $ decodeUtf8 $ B64.encode $ BL.toStrict body1
  _ <- R.liftRedis $ R.setex ("c:" <> B.pack cid <> ":" <> B.pack code) 300 "true"
  return Captcha{..}




